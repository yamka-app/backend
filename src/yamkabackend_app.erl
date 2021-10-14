%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(yamkabackend_app).
-behaviour(application).
-author("Yamka").
-license("MPL-2.0").
-description("The main file").

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    % connect to the Cassandra cluster
    Addr = yamka_config:get(cassandra),
    {ok, Password} = file:read_file("/run/secrets/cassandra_password"),
    {ok, Cassandra} = cqerl:get_client(Addr, [
        {auth, {cqerl_auth_plain_handler, [{"yamkadb", Password}]}},
        {keyspace, "yamkadb"}
    ]),

    yamkabackend_sup:start_link([Cassandra]).

stop(_State) ->
    ok.