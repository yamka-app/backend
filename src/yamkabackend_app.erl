%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(yamkabackend_app).
-behaviour(application).
-author("Yamka").
-license("MPL-2.0").
-description("The main file").

-define(CASSANDRA_IP, "elassandra").
-define(CASSANDRA_PORT, 9042).

-export([start/2, stop/1, app_worker/0]).

app_worker() ->
    {ok, _} = logging:start(),
    {ok, _} = tasty_sup:start_link(),
    {ok, _} = email:start(),

    % connect to the Cassandra cluster
    {ok, Password} = file:read_file("/run/secrets/cassandra_password"),
    {ok, Cassandra} = cqerl:get_client({?CASSANDRA_IP, ?CASSANDRA_PORT}, [
        {auth, {cqerl_auth_plain_handler, [{"yamkadb", Password}]}},
        {keyspace, "yamkadb"}
       ]),
    logging:log("Connected to the Cassandra node at ~s:~p", [?CASSANDRA_IP, ?CASSANDRA_PORT]),

    % start protocol listeners
    ssl:start(),
    spawn_monitor(listeners, normal_listener, [
        Cassandra,
        "/run/secrets/tls_fullchain",
        "/run/secrets/tls_privkey"
    ]),

    % start stat logger
    spawn_monitor(stats, writer_start, [Cassandra]),

    receive
        die -> ok
    end.

start(_StartType, _StartArgs) ->
    {ok, spawn(?MODULE, app_worker, [])}.

stop(_State) ->
    ok.