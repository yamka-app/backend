-module(entities_packet).
-author("Order").
-license("MPL-2.0").

-include("packet.hrl").
-export([encode/2, decode/2, make/2, make/1]).

encode(#{ entities := Entities }, Proto) when Proto >= 5 -> datatypes:enc_list(Entities, fun entity:encode/1, 2).
decode(P, Proto) when Proto >= 5 -> datatypes:dec_list(P, fun entity:len_decode/1, 2).

make(E, R) -> #packet{ type = entities, reply = R, fields = #{ entities => E } }.
make(E) -> make(E, 0).