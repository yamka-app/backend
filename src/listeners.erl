-module(listeners).
-author("Order").
-license("MPL-2.0").
-description("Accepts TLS clients").

-export([normal_listener/3]).

-define(NormalPort, 1746).

normal_listener_loop(Socket, Cassandra) ->
    % accept the client and spawn the client loop
    { ok, TransportSocket } = ssl:transport_accept(Socket),
    spawn(normal_client, client_init, [TransportSocket, Cassandra]),

    normal_listener_loop(Socket, Cassandra).

normal_listener(Cassandra, CertPath, KeyPath) ->
    % listen for new clients
    { ok, ListenSocket } = ssl:listen(?NormalPort, [
        { certfile,   CertPath },
        { cacertfile, CertPath },
        { keyfile,    KeyPath },
        { verify,     verify_none },
        { reuseaddr,  true },
        { versions,   ['tlsv1.2', 'tlsv1.3'] },
        { active,     false }
       ]),

    logging:log("Normal server listening on port ~w", [?NormalPort]),

    normal_listener_loop(ListenSocket, Cassandra).