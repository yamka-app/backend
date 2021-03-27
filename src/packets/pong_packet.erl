%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(pong_packet).
-author("Yamka").
-license("MPL-2.0").

-export([encode/2]).

encode(F, Proto) when Proto >= 5 -> datatypes:enc_num(maps:get(echo, F), 4).