-module(pong_packet).
-author("Order").
-license("MPL-2.0").

-export([encode/2]).

encode(F, Proto) when Proto >= 5 -> datatypes:enc_num(maps:get(echo, F), 4).