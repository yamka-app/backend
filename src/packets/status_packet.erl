-module(status_packet).
-author("Order").
-license("MPL-2.0").

-include("packet.hrl").

-export([encode/2, make/2, make/3, make_rate_limiting/1, make_invalid_state/2]).

encode(F, _P) ->
    Code = datatypes:enc_num(maps:get(maps:get(code, F), ?REVERSE_STATUS_CODE_MAP), 2),
    Msg  = datatypes:enc_str(maps:get(msg, F)),
    <<Code/binary, Msg/binary>>.

make(C, M) ->
    #packet{type = status, fields = #{code => C, msg => M}}.

make(C, M, R) when is_integer(R) ->
    #packet{type = status, reply = R, fields = #{code => C, msg => M}};
make(C, M, R) when is_record(R, packet) ->
    make(C, M, R#packet.seq).

make_rate_limiting(R) ->
    make(rate_limiting, "Rate limiting", R).

make_invalid_state(S, R) ->
    make(invalid_connection_state, lists:flatten(io_lib:format("Connection must be in the \"~p\" state", [S])), R).