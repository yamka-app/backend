%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(sweet_sup).
-behaviour(supervisor).
-author("Yamka").
-license("MPL-2.0").
-description("Per-client supervisor").

-export([start_link/2, init/1]).

start_link(C, S) -> supervisor:start_link(?MODULE, [C, S]).

init([Cassandra, Conn={_, _}]) ->
    % we need this "fake_start_link" trickery to link the main process with its
    % companions logically
    {ok, Main} = sweet_main:start_link(Conn, Cassandra),

    Flags = #{strategy => one_for_one, auto_shutdown => any_significant},
    Children = [
        #{id => main, significant => true, restart => temporary,
          start => {sweet_main, fake_start_link, [Main]}},

        #{id => decoder, significant => true, restart => temporary,
          start => {sweet_decoder, start_link, [Main, Conn]}},

        #{id => encoder, significant => true, restart => temporary,
          start => {sweet_encoder, start_link, [Main, Conn]}}
    ],
    {ok, {Flags, Children}}.