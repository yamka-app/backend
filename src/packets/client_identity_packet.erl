%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(client_identity_packet).
-author("Order").
-license("MPL-2.0").

-include("packet.hrl").
-export([encode/2, make/2]).

encode(F, Proto) when Proto >= 5 -> datatypes:enc_num(maps:get(id, F), 8).

make(I, R) -> #packet{type = client_identity, reply = R, fields = #{id => I}}.