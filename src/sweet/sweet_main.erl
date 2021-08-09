%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(sweet_main).
-author("Yamka").
-license("MPL-2.0").
-description("Main server process for each client").

-record(state, {
    encoder :: pid(),
    decoder :: pid(),
    socket :: ssl:socket(),
    cassandra,
    conn_state :: atom(),
    user_info :: tuple(),
    mfa_state :: term(),
    proto_ver :: number(),
    comp :: boolean()
}).

-export([start/2]).
-export([switch_state/2, switch_state/3, send_packet/2, route_packet/3, stop/1,
         ratelimit/2, ratelimit/3, get_state/1]).

spawn_helper(encoder, Socket, Setup={_,_}) -> spawn_monitor(sweet_encoder, start, [self(), Socket, Setup]);
spawn_helper(decoder, Socket, Proto) -> spawn_monitor(sweet_decoder, start, [self(), Socket, Proto]).

%% starts a main process
start(TransportSocket, Cassandra) ->
    % set up TLS
    {ok, Socket} = ssl:handshake(TransportSocket),

    % create rate limiters
    ratelimit:make(encoder_respawn, {1,   1000  }),
    ratelimit:make(decoder_respawn, {3,   1000  }),
    ratelimit:make(packet,          {100, 1000  }),
    ratelimit:make(close,           {105, 1000  }),
    ratelimit:make(login,           {5,   30000 }),
    ratelimit:make(entity,          {300, 1000  }),
    ratelimit:make(message,         {20,  10000 }),
    ratelimit:make(bot_create,      {1,   120000}),
    ratelimit:make(voice,           {2,   1000  }),

    % start some processes
    Encoder = spawn_helper(encoder, Socket, {0, false}),
    Decoder = spawn_helper(decoder, Socket, 0),
    loop(#state{
        encoder = Encoder,
        decoder = Decoder,
        socket = Socket,
        cassandra = Cassandra,
        conn_state = awaiting_identification,
        user_info = {},
        mfa_state = {},
        proto_ver = 0,
        comp = false
    }).

%% main loop
loop(State) ->
    #state{
        encoder = {EncPid, EncRef},
        decoder = {DecPid, DecRef},
        socket = Socket,
        cassandra = Cassandra,
        conn_state = ConnState,
        mfa_state = MfaState,
        proto_ver = ProtoVer,
        comp = SupportsCompression
    } = State,

    receive
        % restart the decoder when it fails
        {'DOWN', DecRef, process, DecPid, Reason} ->
            logging:err("Decoder down (~p)", [Reason]),
            case ratelimit:hit(decoder_respawn) of
                1 ->
                    self() ! {transmit, self(), status_packet:make(packet_parsing_error, "Packet parsing error")},
                    loop(State#state{decoder = spawn_helper(decoder, Socket, ProtoVer)});
                0 ->
                    logging:err("Decoder respawn rate limit reached", []),
                    exit(respawn_limit_reached)
            end;

        % restart the encoder when it fails
        {'DOWN', EncRef, process, EncPid, Reason} ->
            logging:err("Encoder down (~p)", [Reason]),
            case ratelimit:hit(decoder_respawn) of
                1 ->
                    loop(State#state{encoder = spawn_helper(encoder, Socket, {ProtoVer, SupportsCompression})});
                0 ->
                    logging:err("Encoder respawn rate limit reached", []),
                    exit(respawn_limit_reached)
            end;

        % handle packets decoded by the decoder
        {packet, DecPid, Packet} ->
            logging:dbg("--> ~p", [Packet]),
            spawn(sweet_handler, start, [self(), ConnState, ProtoVer, Cassandra, Packet]),
            loop(State);

        % switch the state (and possible the protocol version) of the protocol processes when the handler asks for it
        {switch_state, _From, awaiting_login, {NewVersion, NewCompression}} ->
            loop(State#state{
                decoder = spawn_helper(decoder, Socket, NewVersion),
                encoder = spawn_helper(encoder, Socket, {NewVersion, NewCompression}),
                conn_state = awaiting_login,
                proto_ver = NewVersion,
                comp = NewCompression
            });
        {switch_state, _From, awaiting_mfa, NewMfa} ->
            loop(State#state{conn_state = awaiting_mfa, mfa_state = NewMfa});
        {switch_state, _From, normal, UserInfo} ->
            loop(State#state{conn_state = normal, user_info = UserInfo});
        {switch_state, _From, NewState} ->
            loop(State#state{conn_state = NewState});

        % send packets when asked by a packet handler or another main process
        {transmit, _From, Packet} ->
            logging:dbg("<-- ~p", [Packet]),
            EncPid ! {packet, self(), Packet},
            loop(State);

        % route packets to another main process when asked by a handler process of ours
        {route, _From, DestSpec, Packet} ->
            % TODO
            loop(State);

        % hits a rate limiter
        {ratelimit, From, Name, Times} ->
            From ! {result, self(), ratelimit:hit(Name, Times)};

        % gets the connection state
        {get_state, From} ->
            From ! {result, self(), ConnState, MfaState};

        % shutdown when asked
        {stop, _From} ->
            ssl:close(Socket),
            ok
    end.

%%%
%%% API functions so that other modules don't have to remember the message format spec
%%% 

switch_state(Pid, awaiting_login, Setup={_,_}) -> Pid ! {switch_state, self(), awaiting_login, Setup};
switch_state(Pid, awaiting_mfa, Mfa) -> Pid ! {switch_state, self(), awaiting_mfa, Mfa};
switch_state(Pid, normal, UserInfo) -> Pid ! {switch_state, self(), normal, UserInfo}.
switch_state(Pid, Target) -> Pid ! {switch_state, self(), Target}.

send_packet(Pid, P) -> Pid ! {transmit, self(), P}.

route_packet(Pid, DestSpec, P) -> Pid ! {route, self(), DestSpec, P}.

ratelimit(Pid, Name) ->
    ratelimit(Pid, Name, 1).
ratelimit(Pid, Name, Times) ->
    Pid ! {ratelimit, self(), Name, Times},
    receive
        {result, Pid, Result} -> {ok, Result}
    after 100 ->
        {error, timeout}
    end.

get_state(Pid) ->
    Pid ! {get_state, self()},
    receive
        {result, Pid, Conn, Mfa} -> {ok, Conn, Mfa}
    after 100 ->
        {error, timeout}
    end.

stop(Pid) -> Pid ! {stop, self()}.