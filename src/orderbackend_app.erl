-module(orderbackend_app).
-behaviour(application).
-author("Order").
-license("MPL-2.0").
-description("The main file").

-define(CASSANDRA_IP, "cassandra").
-define(CASSANDRA_PORT, 9042).

-export([start/2, stop/1, app_worker/0]).

app_worker() ->
    {ok, _} = logging:start(),
    {ok, _} = tasty_sup:start_link(),

    % connect to the Cassandra cluster
    {ok, Password} = file:read_file("/run/secrets/cassandra_password"),
    {ok, Cassandra} = cqerl:get_client({?CASSANDRA_IP, ?CASSANDRA_PORT}, [
        {auth, {cqerl_auth_plain_handler, [{"orderdb", Password}]}},
        {keyspace, "orderdb"}
       ]),
    logging:log("Connected to the Cassandra node at ~s:~p", [?CASSANDRA_IP, ?CASSANDRA_PORT]),

    % start protocol listeners
    ssl:start(),
    spawn(listeners, normal_listener, [
        Cassandra,
        "/run/secrets/tls_fullchain",
        "/run/secrets/tls_privkey"
       ]),

    receive
        die -> ok
    end.

start(_StartType, _StartArgs) ->
    {ok, spawn(?MODULE, app_worker, [])}.

stop(_State) ->
    ok.