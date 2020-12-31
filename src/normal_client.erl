-module(normal_client).
-author("Order").
-license("MPL-2.0").
-description("\"Normal protocol\" client thread").

-export([client_init/3]).

client_loop(Socket, Cassandra) ->
    client_loop(Socket, Cassandra).

% client init function
client_init(TransportSocket, CasLogin, CasPass) ->
    % finish the handshake
    { ok, Socket } = ssl:handshake(TransportSocket),
    { ok, { ClientIP, _} } = ssl:peername(Socket),
    logging:log("~w connected to the normal server", [ClientIP]),

    % connect to the Cassandra cluster
    { ok, Cassandra } = cqerl:get_client({}, [{auth, {cqerl_auth_plain_handler, [{CasLogin, CasPass}]}}]),

    % run the client loop
    client_loop(Socket, Cassandra).