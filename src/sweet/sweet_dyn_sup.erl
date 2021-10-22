%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(sweet_dyn_sup).
-behaviour(supervisor).
-author("Yamka").
-license("MPL-2.0").
-description("Sweet dynamic supervisor. Hosts per-client supervisors").

-export([start_link/1, init/1]).
-export([add_client/1]).

start_link(Args) -> supervisor:start_link({local, sweet_dyn_sup}, ?MODULE, Args).

init([Cassandra]) ->
    Flags = #{strategy => simple_one_for_one, intensity => 0},
    Children = [#{
        id => sweet_sup,
        start => {sweet_sup, start_link, [Cassandra]},
        type => supervisor,
        shutdown => infinity,
        restart => temporary
    }],
    {ok, {Flags, Children}}.

add_client(Conn) -> supervisor:start_child(sweet_dyn_sup, [Conn]).