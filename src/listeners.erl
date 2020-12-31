-module(listeners).
-author("Order").
-license("MPL-2.0").
-description("Accepts TLS clients").

-export([normal_listener/4]).

-define(NormalPort, 1746).

normal_listener_loop(Socket, CasLogin, CasPass) ->
    % accept the client and spawn the client loop
    { ok, TransportSocket } = ssl:transport_accept(Socket),
    spawn(normal_client, client_init, [TransportSocket, CasLogin, CasPass]),

    normal_listener_loop(Socket, CasLogin, CasPass).

normal_listener(CasLogin, CasPass, CertPath, KeyPath) ->
    % listen for new clients
    { ok, ListenSocket } = ssl:listen(?NormalPort, [
        { certfile, CertPath },
        { keyfile,  KeyPath }
       ]),

    logging:log("Normal server listening on port ~w", [?NormalPort]),

    normal_listener_loop(ListenSocket, CasLogin, CasPass).