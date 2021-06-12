%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(pkey_e).
-author("Yamka").
-license("MPL-2.0").
-description("The public key entity").

-include("entity.hrl").
-include_lib("cqerl/include/cqerl.hrl").

-export([get/1, create/4, delete/1, get_by_user/2, count/2]).
-export([limit_rate/2, check_rate_limit/2]).

%% gets a public key by ID
get(Id) ->
    {ok, Rows} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT * FROM pkeys WHERE id=?",
        values    = [{id, Id}]
    }),
    1 = cqerl:size(Rows),
    maps:from_list(cqerl:head(Rows)).

%% creates a public key
create(User, Type, Key, Signature) ->
    Id = utils:gen_snowflake(),
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "INSERT INTO pkeys (id, user, type, key, signature) VALUES (?,?,?,?,?)",
        values = [
            {id, Id}, {user, User}, {type, maps:get(Type, ?REVERSE_KEY_TYPE_MAP)},
            {key, Key}, {signature, Signature}
        ]
    }),
    Id.

delete(Id) ->
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "DELETE FROM pkeys WHERE id=?",
        values    = [{id, Id}]
    }).

%% gets user's public keys
get_by_user(Id, Type) ->
    {ok, Rows} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT id FROM pkeys_by_user WHERE user=? AND type=? ALLOW FILTERING",
        values    = [{user, Id}, {type, maps:get(Type, ?REVERSE_KEY_TYPE_MAP)}]
    }),
    [Key || [{id, Key}] <- cqerl:all_rows(Rows)].

%% counts user's keys
count(Id, Type) ->
    % dOnT uSe cOuNt iN pRoDuCtIoN
    % (the dataset is realyl small, I guess we can afford it)
    {ok, Rows} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT COUNT(*) FROM pkeys_by_user WHERE user=? AND type=? ALLOW FILTERING",
        values    = [{user, Id}, {type, maps:get(Type, ?REVERSE_KEY_TYPE_MAP)}]
    }),
    [[{count, Count}]] = cqerl:all_rows(Rows),
    Count.

limit_rate(Subj, User) ->
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        % assuming default_time_to_live is configured for this table
        statement = "INSERT INTO key_limiter (subject, user) VALUES (?,?)",
        values = [{subject, Subj}, {user, User}]
    }).

check_rate_limit(Subj, User) ->
    {ok, Rows} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT * FROM key_limiter WHERE subject=? AND user=?",
        values    = [{subject, Subj}, {user, User}]
    }),
    length(cqerl:all_rows(Rows)) =:= 0.