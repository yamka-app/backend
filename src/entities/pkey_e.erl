%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(pkey_e).
-author("Yamka").
-license("MPL-2.0").
-description("The public key entity").

-include_lib("cqerl/include/cqerl.hrl").

-export([get/1, create/4]).

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
        values = [{id, Id}, {user, User}, {type, Type}, {key, Key}, {signature, Signature}]
    }),
    Id.