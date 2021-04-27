%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(search_result_packet).
-author("Yamka").
-license("MPL-2.0").

-include("packet.hrl").
-export([encode/2, make/2]).

encode(#{list := List}, Proto) when Proto >= 5 ->
    datatypes:enc_num_list(List, 8).

make(L, R) -> #packet{type = search_result, reply = R, fields = #{list => L}}.