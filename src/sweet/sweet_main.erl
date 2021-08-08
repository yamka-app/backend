%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(sweet_main).
-author("Yamka").
-license("MPL-2.0").
-description("Main server process for each client").

-define(MIN_PROTO, 12). % lowest and highest allowed versions
-define(MAX_PROTO, 14).

-record(state, {
    encoder :: pid(),
    decoder :: pid(),
    socket :: ssl:socket(),
    cassandra,
    conn_state :: atom(),
    proto_ver :: number()
}).

-export([start/2]).
-export([switch_state/2, switch_state/3, send_packet/2, route_packet/3, stop/1]).

spawn_helper(encoder, Socket, Proto) -> spawn_monitor(sweet_encoder, start, [self(), Socket, Proto]);
spawn_helper(decoder, Socket, Proto) -> spawn_monitor(sweet_decoder, start, [self(), Socket, Proto]).

%% starts a main process
start(Socket, Cassandra) ->
    % start some processes
    Encoder = spawn_helper(encoder, Socket, 12),
    Decoder = spawn_helper(decoder, Socket, 12),
    loop(#state{
        encoder = Encoder,
        decoder = Decoder,
        socket = Socket,
        cassandra = Cassandra,
        conn_state = awaiting_identification
    }).

%% main loop
loop(State) ->
    #state{
        encoder = {EncPid, EncRef},
        decoder = {DecPid, DecRef},
        socket = Socket,
        cassandra = Cassandra,
        conn_state = ConnState,
        proto_ver = ProtoVer
    } = State,

    receive
        % restart the decoder when it fails
        {'DOWN', DecRef, process, DecPid, Reason} ->
            logging:err("Decoder down (~p)", [Reason]),
            self() ! {transmit, self(), status_packet:make(packet_parsing_error, "Packet parsing error")},
            loop(State#state{decoder = spawn_helper(decoder, Socket, ProtoVer)});

        % restart the encoder when it fails
        {'DOWN', EncRef, process, EncPid, Reason} ->
            logging:err("Encoder down (~p)", [Reason]),
            loop(State#state{encoder = spawn_helper(encoder, Socket, ProtoVer)});

        % handle packets decoded by the decoder
        {packet, DecPid, Packet} ->
            logging:dbg("--> ~p", [Packet]),
            spawn(sweet_handler, start, [self(), ConnState, ProtoVer, Cassandra]),
            loop(State);

        % switch the state (and possible the protocol version) of the protocol processes when the handler asks for it
        {switch_state, _From, awaiting_login, NewVersion} ->
            loop(State#state{
                decoder = spawn_helper(decoder, Socket, ProtoVer),
                encoder = spawn_helper(encoder, Socket, ProtoVer),
                proto_ver = NewVersion
            });
        {switch_state, _From, NewState} ->
            loop(State#state{conn_state = NewState});

        % send packets when asked by a packet handler or another main process
        {transmit, _From, Packet} ->
            logging:dbg("<-- ~p", [Packet]),
            EncPid ! {packet, self(), Packet},
            loop(State);

        % route packets to another main process when asked by a handler process of ours
        {route, _From, DestSpec, Packet} ->
            % stub
            loop(State);

        % shutdown when asked
        {stop, _From} ->
            ok
    end.

%%%
%%% API functions so that other modules don't have to remember the message format spec
%%% 

switch_state(Pid, awaiting_login, Ver) -> Pid ! {switch_state, self(), awaiting_login, Ver}.
switch_state(Pid, Target) -> Pid ! {switch_state, self(), Target}.

send_packet(Pid, P) -> Pid ! {transmit, self(), P}.

route_packet(Pid, DestSpec, P) -> Pid ! {route, self(), DestSpec, P}.

stop(Pid) -> Pid ! {stop, self()}.