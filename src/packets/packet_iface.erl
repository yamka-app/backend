-module(packet_iface).
-author("Order").
-license("MPL-2.0").
-description("Packet encoding and decoding").

-include("packet.hrl").
-define(COMPRESSION_THRESHOLD, 128). % used when sending only

-export([reader/3, writer/6]).

%% decodes a packet
decode(Data, ProtocolVersion) ->
    TypeCode = datatypes:dec_num(binary:part(Data, 0, 1)),
    Seq      = datatypes:dec_num(binary:part(Data, 1, 4)),
    Reply    = datatypes:dec_num(binary:part(Data, 5, 4)),
    Captcha  = datatypes:dec_str(binary:part(Data, 9, 9, byte_size(Data) - 9)),
    CapLen   = datatypes:len_str(binary:part(Data, 9, 9, byte_size(Data) - 9)),
    Payload  = binary:part(Data, 9 + CapLen, byte_size(Data) - 9 - CapLen),

    Type = try maps:get(TypeCode, ?PACKET_TYPE_MAP) of
        Val -> Val
    catch
        _:_ -> unknown
    end,

    %% try to parse the packet payload
    %% evaluate to { ok, #packet } in case it succeeded,
    %% evaluate to { error, Seq } in case it failed
    try case Type of
        login          -> login_packet:decode(Payload, ProtocolVersion);
        identification -> identification_packet:decode(Payload, ProtocolVersion);
        _ -> #{ } end
    of
        F -> { ok, #packet{ type    = Type,
                            seq     = Seq,
                            reply   = Reply,
                            captcha = Captcha,
                            fields  = F } }
    catch
        E:D:T -> { error, Seq, Type, { E, D, T } }
    end.

%% encodes a packet
encode(Packet, ProtocolVersion) ->
    Fields = Packet#packet.fields,
    Payload = case Packet#packet.type of
        status -> status_packet:encode(Fields, ProtocolVersion)
    end,
    
    Type     = datatypes:enc_num(maps:get(Packet#packet.type, ?REVERSE_PACKET_TYPE_MAP), 1),
    SeqBin   = datatypes:enc_num(Packet#packet.seq, 4),
    ReplyBin = datatypes:enc_num(Packet#packet.reply, 4),

    <<Type/binary, SeqBin/binary, ReplyBin/binary, Payload/binary>>.

%% reads a packet complete with decompression logic
reader(Socket, Protocol, Pid) ->
    % read the compression header
    { ok, CHdrList } = ssl:recv(Socket, 5),
    CHdr = list_to_binary(CHdrList),
    Compressed    = datatypes:dec_bool(binary:part(CHdr, 0, 1)),
    CompressedLen = datatypes:dec_num (binary:part(CHdr, 1, 4)),

    % read the (possibly compressed) data header and payload
    { ok, CDataList } = ssl:recv(Socket, CompressedLen),
    CData = list_to_binary(CDataList),
    Data = if
        Compressed ->
            Z = zlib:open(),
            ok = zlib:inflateInit(Z, default),
            D = zlib:inflate(Z, CData),
            zlib:inflateEnd(Z), D;
        true -> CData
    end,

    % decode data and send it
    Pid ! case decode(Data, Protocol) of
        { ok, P }          -> { packet, P };
        { error, S, T, E } -> { decoding_error, S, T, E }
    end.

%% writes a packet coomplete with compression logic
writer(Socket, Packet, Seq, Proto, SupportsCompression, Pid) ->
    %% encode data
    Packet#packet{ seq = Seq },
    Data = encode(Packet, Proto),

    %% compress it if necessary
    Compressed = (byte_size(Data) >= ?COMPRESSION_THRESHOLD) and SupportsCompression,
    CData = if
        Compressed ->
            Z = zlib:open(),
            ok = zlib:deflateInit(Z, best_compression),
            D = iolist_to_binary(zlib:deflate(Z, Data, finish)),
            zlib:deflateEnd(Z), D;
        true -> Data
    end,

    %% add the compression header
    CBin = datatypes:enc_bool(Compressed),
    CLBin = datatypes:enc_num(byte_size(CData), 4),
    ssl:send(Socket, <<CBin/binary, CLBin/binary, CData/binary>>),
    
    Pid ! { sent, Seq }.