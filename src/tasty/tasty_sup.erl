%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(tasty_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    {ok, {{one_for_one, 5, 60},
        [{tasty,
            {tasty, start, []},
            permanent,
            5000,
            worker,
            [tasty]},
         {tasty_listener,
            {tasty_listener, start, []},
            permanent,
            5000,
            worker,
            [tasty_listener]}]}}.