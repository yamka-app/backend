-module(access_token_packet).
-author("Order").
-license("MPL-2.0").

-include("packet.hrl").
-export([encode/2, decode/2, make/2]).

encode(F, Proto) when Proto >= 5 -> datatypes:enc_str(maps:get(token, F)).
decode(P, Proto) when Proto >= 5 -> #{token => datatypes:dec_str(P)}.

make(T, R) -> #packet{type = access_token, reply = R, fields = #{token => T}}.