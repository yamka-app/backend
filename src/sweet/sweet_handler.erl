%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(sweet_handler).
-author("Yamka").
-license("MPL-2.0").
-description("Packet handler process for each packet").

-include("../packets/packet.hrl").

-export([start/4]).

start(Main, ConnState, ProtoVer, Cassandra) ->
    ok.