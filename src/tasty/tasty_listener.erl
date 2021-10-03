%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(tasty_listener).
-author("Yamka").
-license("MPL-2.0").
-description("The Tasty (voice/video protocol) UDP listener").

-export([start/0]).
-export([init/0, run/1]).
-record(state, {socket}).

start() -> {ok, spawn_link(?MODULE, init, [])}.

init() ->
    Port = application:get_env(yamkabackend, tasty_port),
    {ok, Socket} = gen_udp:open(Port, [inet, inet6, binary, {tos, 184}]),
    logging:log("Tasty listener running (node ~p, port ~p)", [node(), Port]),
    register(tasty_listener, self()),

    run(#state{socket=Socket}).

run(State = #state{socket=Socket}) ->
    receive
        {udp, Socket, IP, Port, Packet} ->
            tasty_client:handle_packet({IP, Port}, Packet);
        {send, {IP, Port}, Data} ->
            gen_udp:send(Socket, IP, Port, Data)
    end,
    run(State).