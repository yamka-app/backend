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
    socket,
    cassandra
}).

-export([start/2]).

start(Socket, Cassandra) ->
    % start some processes
    Encoder = spawn_monitor(sweet_proto, encoder_start, []),
    Decoder = spawn_monitor(sweet_proto, decoder_start, []),
    loop(#state{encoder=Encoder, decoder=Decoder}).

loop(State) ->
    #state{encoder={EncPid, EncRef}, decoder={DecPid, DecRef}} = State,

    receive
        % restart the decoder when it fails
        {'DOWN', DecRef, process, DecPid, Reason} ->
            logging:err("Decoder down (~p)", [Reason]),
            self() ! {transmit, self(), status_packet:make(packet_parsing_error, "Packet parsing error")},
            loop(State#state{decoder=spawn_monitor(sweet_proto, decoder_start, [])});

        % restart the encoder when it fails
        {'DOWN', EncRef, process, EncPid, Reason} ->
            logging:err("Encoder down (~p)", [Reason]),
            loop(State#state{encoder=spawn_monitor(sweet_proto, encoder_start, [])});

        % handle packets decoded by the decoder
        {packet, DecPid, Packet} ->
            logging:dbg("--> ~p", [Packet]),
            loop(State);

        % send packets when asked by a packet handler or another main process
        {transmit, _From, Packet} ->
            EncPid ! {packet, self(), Packet},
            loop(State)
    end.