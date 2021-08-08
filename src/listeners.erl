%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(listeners).
-author("Yamka").
-license("MPL-2.0").
-description("Accepts TLS clients").

-export([sweet_listener/3, client_count/0, stop/0, start/1]).
-export([setup_tables/0, destroy_tables/0]).

-define(NORMAL_PORT, 1746).

client_count() -> length(ets:match(id_of_processes, '_')).

cleanup(Pid) ->
    [{Pid, Id, Agent}] = ets:lookup(id_of_processes, Pid),
    ets:match_delete(id_of_processes, {Pid, Id, Agent}),
    ets:match_delete(icpc_processes,  {Id, Agent, '_'}),
    ets:match_delete(user_awareness,  {'_', {Id, Pid}}),
    ets:match_delete(chan_awareness,  {'_', {Id, Pid}}),
    ets:match_delete(poll_awareness,  {'_', {Id, Pid}}),
    ets:match_delete(file_awareness,  {'_', {Id, Pid}}),
    ets:match_delete(group_awareness, {'_', {Id, Pid}}),
    ets:match_delete(typing,          {'_', {Id, '_'}}),
    user_e:broadcast_status(Id, offline).

sweet_listener_server() ->
    receive
        {start, Args} ->
            spawn_monitor(client, client_init, Args),
            sweet_listener_server();
        {stop} ->
            logging:log("Sweet server shutting down", []),
            ok;
        {'DOWN', _, process, Pid, _} ->
            spawn(fun() -> cleanup(Pid) end),
            logging:log("Sweet server: ~p died", [Pid]),
            sweet_listener_server()
    end.

sweet_listener_loop(Socket, Cassandra) ->
    % accept the client and spawn the client loop
    {ok, TransportSocket} = ssl:transport_accept(Socket),
    sweet_server ! {start, [TransportSocket, Cassandra]},
    sweet_listener_loop(Socket, Cassandra).

sweet_listener(Cassandra, CertPath, KeyPath) ->
    % start the listener message server
    register(sweet_server, spawn(fun sweet_listener_server/0)),

    % create a handful of tables
    setup_tables(),

    % listen for new clients
    {ok, ListenSocket} = ssl:listen(?NORMAL_PORT, [
        {certfile,   CertPath},
        {cacertfile, CertPath},
        {keyfile,    KeyPath},
        {verify,     verify_none},
        {reuseaddr,  true},
        {versions,   ['tlsv1.2', 'tlsv1.3']},
        {active,     false}
    ]),

    logging:log("Sweet server listening on port ~w", [?NORMAL_PORT]),
    sweet_listener_loop(ListenSocket, Cassandra).

start(Cassandra) ->
    ssl:start(),
    {Pid, _Mon} = spawn_monitor(listeners, sweet_listener, [
        Cassandra,
        "/run/secrets/tls_fullchain",
        "/run/secrets/tls_privkey"
    ]),
    register(sweet_listener, Pid).

stop() ->
    sweet_listener ! {'EXIT', self(), normal},
    unregister(sweet_listener),
    sweet_server ! {'EXIT', self(), normal},
    unregister(sweet_server),
    destroy_tables(),
    ssl:stop().

setup_tables() ->
    ets:new(id_of_processes, [set, public, named_table]),
    ets:new(icpc_processes,  [bag, public, named_table]),
    ets:new(user_awareness,  [bag, public, named_table]),
    ets:new(chan_awareness,  [bag, public, named_table]),
    ets:new(poll_awareness,  [bag, public, named_table]),
    ets:new(file_awareness,  [bag, public, named_table]),
    ets:new(group_awareness, [bag, public, named_table]),
    ets:new(typing,          [bag, public, named_table]).

destroy_tables() ->
    ets:delete(id_of_processes),
    ets:delete(icpc_processes),
    ets:delete(user_awareness),
    ets:delete(chan_awareness),
    ets:delete(poll_awareness),
    ets:delete(file_awareness),
    ets:delete(group_awareness),
    ets:delete(typing).