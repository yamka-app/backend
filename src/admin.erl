%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(admin).
-author("Yamka").
-license("MPL-2.0").
-description("Console administration commands").

-export([seed_nodes/1, powerdown/0, powerup/0]).

%% connects to nodes in the cluster
seed_nodes(Nodes) when is_list(Nodes) ->
    logging:log("Seeding nodes: ~p", [Nodes]),
    [net_kernel:connect_nodes(Node) || Node <- Nodes],
    logging:log("Now connected to: ~p", [nodes()]),
    ok.

%% starts accepting Sweet connections and performs other startup tasks
powerup() ->
    yamkabackend_app:powerup().

%% stops accepting Sweet connections and performs other shutdown tasks
powerdown() ->
    yamkabackend_app:powerdown().