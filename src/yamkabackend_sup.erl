%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(yamkabackend_sup).
-behaviour(supervisor).
-author("Yamka").
-license("MPL-2.0").
-description("Main supervisor").

-export([start_link/1, init/1]).

start_link(Args) -> supervisor:start_link({local, main_sup}, ?MODULE, Args).

init([Cassandra]) ->
    Flags = #{strategy => one_for_one},
    Children = [
        #{id => tasty, type => supervisor, shutdown => infinity,
          start => {tasty_sup, start_link, []}},

        #{id => email,
          start => {email, start_link, []}},

        #{id => stat_logger,
          start => {stats, start_link, [Cassandra]}},

        #{id => awareness_server,
          start => {sweet_awareness, start_link, [Cassandra]}},
          
        #{id => ownership_server,
          start => {sweet_owners, start_link, [Cassandra]}},
          
        #{id => sweet_dyn_sup, type => supervisor, shutdown => infinity,
          start => {sweet_dyn_sup, start_link, [[Cassandra]]}},

        #{id => sweet_listener,
          start => {sweet_listener, start_link, [Cassandra]}}
    ],
    {ok, {Flags, Children}}.