%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(entities_packet).
-author("Yamka").
-license("MPL-2.0").

-include("packet.hrl").
-export([encode/2, decode/2, make/2, make/1]).

encode(#{entities := Entities}, Proto) when Proto >= 5 ->
    datatypes:enc_list(Entities, fun(E) -> entity:encode(E, Proto) end, 2).

decode(P, Proto) when Proto >= 5 ->
    #{entities => datatypes:dec_list(P, fun(D) -> entity:len_decode(D, Proto) end, 2)}.

make(E, R) -> #packet{type = entities, reply = R, fields = #{entities => E}}.
make(E) -> make(E, 0).