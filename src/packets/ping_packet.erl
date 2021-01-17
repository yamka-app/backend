-module(ping_packet).
-author("Order").
-license("MPL-2.0").

-export([decode/2]).

decode(Payload, ProtocolVersion) when ProtocolVersion >= 5 ->
    4 = byte_size(Payload),
    Echo = datatypes:dec_num(Payload),
    #{ echo => Echo }.