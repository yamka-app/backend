%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(search_packet).
-author("Yamka").
-license("MPL-2.0").

-include("packet.hrl").
-export([decode/2]).

decode(<<Type:8/integer, Ref:64/unsigned, Name/binary>>, ProtocolVersion) when ProtocolVersion >= 5 ->
  #{type => maps:get(Type, ?SEARCH_TARGET_MAP),
    ref  => Ref,
    name => datatypes:dec_str(Name)}.