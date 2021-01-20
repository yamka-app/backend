-module(client_identity_packet).
-author("Order").
-license("MPL-2.0").

-include("packet.hrl").
-export([encode/2, make/2]).

encode(F, Proto) when Proto >= 5 -> datatypes:enc_num(maps:get(id, F), 8).

make(I, R) -> #packet{type = client_identity, reply = R, fields = #{id => I}}.