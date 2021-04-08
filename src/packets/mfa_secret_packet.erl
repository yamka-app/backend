%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(mfa_secret_packet).
-author("Yamka").
-license("MPL-2.0").

-include("packet.hrl").
-export([encode/2, decode/2, make/2]).

encode(F, Proto) when Proto >= 5 -> datatypes:enc_str(maps:get(secret, F)).
decode(P, Proto) when Proto >= 5 -> #{secret => datatypes:dec_str(P)}.

make(T, R) -> #packet{type = mfa_secret, reply = R, fields = #{secret => T}}.