%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(agent_e).
-author("Yamka").
-license("MPL-2.0").
-description("The agent entity. An agent is a device bound to a specific user").

-include_lib("cqerl/include/cqerl.hrl").

-export([get/1, create/3, get_by_user/1, delete/1, online/1]).

%% gets an agent by ID
get(Id) ->
    {ok, Rows} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT * FROM agents WHERE id=?",
        values    = [{id, Id}]
    }),
    1 = cqerl:size(Rows),
    maps:from_list(cqerl:head(Rows)).

%% creates an agent
create(Owner, Type, Name) ->
    Id = utils:gen_snowflake(),
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "INSERT INTO agents (id, owner, type, name) VALUES (?,?,?,?)",
        values = [{id, Id}, {owner, Owner}, {type, Type}, {name, Name}]
    }),
    Id.

%% gets all agents a user owns
get_by_user(Id) ->
    {ok, Result} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT id FROM agents_by_user WHERE owner=?",
        values = [{owner, Id}]
    }),
    lists:sort([S || [{id, S}] <- cqerl:all_rows(Result)]).

delete(Id) ->
    cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "DELETE FROM agents WHERE id=?",
        values = [{id, Id}]
    }).

online(Id) ->
    #{owner := User} = agent_e:get(Id),
    AllClients = ets:lookup(icpc_processes, User),
    length(lists:filter(fun({_, A, _}) -> A =:= Id end, AllClients)) > 0.