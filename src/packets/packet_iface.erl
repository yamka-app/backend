-module(packet_iface).
-author("Order").
-license("MPL-2.0").
-description("Packet encoding and decoding").

-include("packet.hrl").

-export([decode/1, encode/3]).

%% decodes a packet
decode(Data) ->
    TypeCode = datatypes:dec_num(binary:part(Data, 0, 1)),
    Seq      = datatypes:dec_num(binary:part(Data, 1, 4)),
    Reply    = datatypes:dec_num(binary:part(Data, 5, 4)),
    Payload  = binary:part(Data, 9, byte_size(Data) - 9),

    Type = try maps:get(TypeCode, ?PACKET_TYPE_MAP) of
        Val -> Val
    catch
        badkey:_ -> unknown
    end,

    %% try to parse the payload
    %% evaluate to { ok, #packet } in case it succeeded,
    %% evaluate to { error, Seq } in case it failed
    try case Type of
        login          -> login_packet:decode(Payload);
        identification -> identification_packet:decode(Payload);
        _ -> { ok, #packet{ type = unknown, seq = Seq, reply = Reply, fields = #{ } } } end
    of
        F -> { ok, #packet{ type   = Type,
                            seq    = Seq,
                            reply  = Reply,
                            fields = F } }
    catch
        _ -> { error, Seq }
    end.

%% encodes a packet
encode(Packet, Seq, Reply) ->
    Payload = case Packet#packet.type of
        status -> status_packet:encode(Packet#packet.fields)
    end,
    
    Type     = datatypes:enc_num(maps:get(Packet#packet.type, ?REVERSE_PACKET_TYPE_MAP), 1),
    SeqBin   = datatypes:enc_num(Seq, 4),
    ReplyBin = datatypes:enc_num(Reply, 4),

    <<Type/binary, SeqBin/binary, ReplyBin/binary, Payload/binary>>.