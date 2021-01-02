-module(packet_iface).
-author("Order").
-license("MPL-2.0").
-description("Packet encoding and decoding").

-include("packet.hrl").

-export([decode/1]).

% decodes a packet
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

    Fields = case Type of
        login -> login_packet:decode();
        _ -> []
    end,

    { ok, #packet{ type   = Type,
             seq    = Seq,
             reply  = Reply,
             fields = Fields } }.