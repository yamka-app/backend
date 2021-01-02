-module(normal_client).
-author("Order").
-license("MPL-2.0").
-description("\"Normal protocol\" client process").

-define(TIMEOUT, 30*1000).

-export([client_init/2]).

% the client loop
% reads the client's packets and responds to them
client_loop(Socket, Cassandra) ->
    % read the compression header
    { ok, CHdrList } = ssl:recv(Socket, 5, ?TIMEOUT),
    CHdr = list_to_binary(CHdrList),
    Compressed    = datatypes:dec_bool(binary:part(CHdr, 0, 1)),
    CompressedLen = datatypes:dec_num (binary:part(CHdr, 1, 4)),

    % read the (possibly compressed) data header and payload
    { ok, CDataList } = ssl:recv(Socket, CompressedLen, ?TIMEOUT),
    CData = list_to_binary(CDataList),
    Data = if
        Compressed ->
            Z = zlib:open(),
            ok = zlib:inflateInit(Z, default),
            zlib:inflate(Z, CData);
        true -> CData
    end,

    % decode the packet
    { ok, Packet } = packet_iface:decode(Data),

    client_loop(Socket, Cassandra).

% client init function
client_init(TransportSocket, Cassandra) ->
    % finish the handshake
    { ok, Socket } = ssl:handshake(TransportSocket),
    { ok, { ClientIP, _ } } = ssl:peername(Socket),
    logging:log("~w connected to the normal server", [ClientIP]),

    % run the client loop
    client_loop(Socket, Cassandra).