-module(normal_client).
-author("Order").
-license("MPL-2.0").
-description("\"Normal protocol\" client process").

-define(TIMEOUT, 30*1000).
-define(MIN_PROTO, 5).
-define(MAX_PROTO, 5).
-define(COMPRESSION_THRESHOLD, 32).
-include("packets/packet.hrl").

-export([client_init/2]).

send_packet(Socket, P, Seq, Reply) ->
    %% encode data
    Data = packet_ifce:ecnode(P, Seq, Reply),

    %% compress it if necessary
    Compressed = byte_size(Data) >= ?COMPRESSION_THRESHOLD,
    CData = if
        Compressed ->
            Z = zlib:open(),
            ok = zlib:deflateInit(Z, default),
            zlib:deflate(Z, Data);
        true -> Data
    end,

    %% add the compression header
    CBin = datatypes:enc_bool(Compressed),
    CLBin = byte_size(CData),
    ssl:send(Socket, <<CBin/binary, CLBin/binary, CData/binary>>).

% the client loop
% reads the client's packets and responds to them
client_loop(Socket, Cassandra, NextSeq, State) ->
    %% read the compression header
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

    %% decode the packet
    { ok, { ToSend, Reply }, Seq } = case packet_iface:decode(Data) of
        { ok, Packet } ->
            %% every statement in this case switch is expected to return...
            %% this ^^^^^^^^ mess... Yeah. That's so we can keep trak of the
            %% current SEQ number (packet sequential ID) and reply to a client's
            %% packet with ease
            case Packet#packet.type of
                %% the client told us their protocol version
                identification ->
                    Proto = maps:get(protocol, Packet#packet.fields),
                    if
                        (Proto < ?MIN_PROTO) or (Proto > ?MAX_PROTO) -> { ok, { #packet{
                            type = status, fields = #{ code => unsupported_proto, msg => "Unsupported protocol version" } },
                            Packet#packet.seq }, NextSeq + 1 };
                        true -> maps:put(protocol, Proto, State), { ok, { none, none }, NextSeq }
                    end;
                _ -> { ok, { none, none }, NextSeq }
            end;
        
        { error, PacketSeq } ->
            { ok, { #packet{ type = status, fields = #{
                code => packet_parsing_error,
                msg => "Packet parsing failed. Check structures with the documentation: https://docs.ordermsg.tk" } },
                PacketSeq }, NextSeq + 1 }
    end,

    %% send a reply packet if needed
    case ToSend of
        none -> ok;
        P    -> send_packet(Socket, P, Seq, Reply)
    end,

    client_loop(Socket, Cassandra, Seq, State).

% client init function
client_init(TransportSocket, Cassandra) ->
    % finish the handshake
    { ok, Socket } = ssl:handshake(TransportSocket),
    { ok, { ClientIP, _ } } = ssl:peername(Socket),
    logging:log("~w connected to the normal server", [ClientIP]),

    % run the client loop
    client_loop(Socket, Cassandra, 1, #{ }).