-module(identification_packet).
-author("Order").
-license("MPL-2.0").

-export([decode/1]).

decode(Payload) ->
    4 = byte_size(Payload),
    { ok, #{ protocol => datatypes:dec_num(binary:part(Payload, 0, 4)) } }.