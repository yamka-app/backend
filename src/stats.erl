%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(stats).
-author("Yamka").
-license("MPL-2.0").
-description("Stats collection").

-include_lib("cqerl/include/cqerl.hrl").

-export([start_link/1, stats/0, writer_start/1]).

stats() ->
    #{
        processes => erlang:system_info(process_count),
        atoms     => erlang:system_info(atom_count),
        clients   => sweet_listener:client_count(),
        io        => erlang:statistics(io)
    }.

write_current() ->
    lager:info("writing current stats", []),

    #{clients := Clients} = stats(),
    {ok, _} = cqerl:run_query(get(cassandra), #cql_query{
        statement = "INSERT INTO clients_by_time (node, at, clients) values (?, dateof(now()), ?)",
        values    = [{node, node()}, {clients, Clients}]
    }).

start_link(Cassandra) -> {ok, spawn_link(?MODULE, writer_start, [Cassandra])}.

writer_start(Cassandra) ->
    lager:info("Stat logger started", []),
    put(cassandra, Cassandra),
    writer().

writer() ->
    receive
        _ -> ok
    after yamka_config:get(stat_interval) ->
        write_current()
    end,
    writer().