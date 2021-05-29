%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(client_identity_packet).
-author("Yamka").
-license("MPL-2.0").

-include("packet.hrl").
-export([encode/2, make/3]).

encode(#{user := User, agent := Agent}, Proto) when Proto >= 9 ->
    <<User:64/unsigned, Agent:64/unsigned>>.

make(U, A, R) -> #packet{type = client_identity, reply = R, fields = #{user => U, agent => A}}.