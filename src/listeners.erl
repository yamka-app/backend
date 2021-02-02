-module(listeners).
-author("Order").
-license("MPL-2.0").
-description("Accepts TLS clients").

-export([normal_listener/3]).

-define(NormalPort, 1746).

cleanup(Pid) ->
    [{Pid, Id}] = ets:lookup(id_of_processes, Pid),
    ets:match_delete(user_awareness, {'_', {Id, Pid}}).

listener_server() ->
    receive
        {start, Args}                -> spawn_monitor(normal_client, client_init, Args);
        {'DOWN', _, process, Pid, _} ->
            spawn(fun() -> cleanup(Pid) end),
            logging:log("Listener server: ~p died", [Pid])
    end,
    listener_server().

normal_listener_loop(Socket, Cassandra) ->
    % accept the client and spawn the client loop
    {ok, TransportSocket} = ssl:transport_accept(Socket),
    listener_server ! {start, [TransportSocket, Cassandra]},
    normal_listener_loop(Socket, Cassandra).

normal_listener(Cassandra, CertPath, KeyPath) ->
    % start the listener message server
    register(listener_server, spawn(fun listener_server/0)),
    % create a handful of tables
    ets:new(id_of_processes, [set, public, named_table]),
    ets:new(icpc_processes,  [bag, public, named_table]),
    ets:new(user_awareness,  [bag, public, named_table]),
    % listen for new clients
    {ok, ListenSocket} = ssl:listen(?NormalPort, [
        {certfile,   CertPath},
        {cacertfile, CertPath},
        {keyfile,    KeyPath},
        {verify,     verify_none},
        {reuseaddr,  true},
        {versions,   ['tlsv1.2', 'tlsv1.3']},
        {active,     false}
       ]),

    logging:log("Normal server listening on port ~w", [?NormalPort]),

    normal_listener_loop(ListenSocket, Cassandra).