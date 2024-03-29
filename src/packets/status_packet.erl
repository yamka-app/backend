%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(status_packet).
-author("Yamka").
-license("MPL-2.0").

-include("packet.hrl").

-export([encode/2, make/2, make/3]).
-export([make_rate_limiting/0, make_invalid_state/1, make_excessive_data/0]).

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

make_rate_limiting() ->
    make(rate_limiting, "Rate limiting").

make_invalid_state(S) ->
    make(invalid_connection_state, lists:flatten(io_lib:format("Connection must be in the \"~p\" state", [S]))).

make_excessive_data() -> make(excessive_data, "Excessive data").