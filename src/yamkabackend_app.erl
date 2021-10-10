%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(yamkabackend_app).
-behaviour(application).
-author("Yamka").
-license("MPL-2.0").
-description("The main file").

-export([powerup/1, powerdown/0]).
-export([start/2, stop/1, app_worker/0]).

powerup(Port) ->
    % connect to the Cassandra cluster
    {ok, Password} = file:read_file("/run/secrets/cassandra_password"),
    {ok, Cassandra} = cqerl:get_client(yamka_config:get(cassandra), [
        {auth, {cqerl_auth_plain_handler, [{"yamkadb", Password}]}},
        {keyspace, "yamkadb"}
    ]),
    logging:log("Connected to the Cassandra node at ~p", [yamka_config:get(cassandra)]),

    % start protocol listeners
    tasty_sup:start_link(),
    sweet_listener:start(Cassandra, Port),

    % start stat logger
    register(stat_logger, spawn(stats, writer_start, [Cassandra])),
    ok.

powerdown() ->
    stat_logger ! {'EXIT', self(), normal},
    unregister(stat_logger),
    sweet_listener:stop(),
    ok.

app_worker() ->
    app_worker().

start(_StartType, _StartArgs) ->
    sync:go(),

    {ok, _} = logging:start(),
    {ok, _} = email:start(),

    admin:powerup(),

    {ok, spawn(?MODULE, app_worker, [])}.

stop(_State) ->
    powerdown(),
    logging:stop(),
    email:stop(),
    ok.