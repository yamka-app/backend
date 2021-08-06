%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(file_e). % the _e is here because the "file" exists in OTP
-author("Yamka").
-license("MPL-2.0").
-description("The file entity").

-include_lib("cqerl/include/cqerl.hrl").

-export([get/1, update/2]).

%% gets a file by ID
get(Id) ->
    {ok, Rows} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT * FROM blob_store WHERE id=?",
        values    = [{id, Id}]
    }),
    1 = cqerl:size(Rows),
    Row = maps:from_list(cqerl:head(Rows)),
    maps:map(fun(K, V) ->
        case K of
            emoji_name when V =:= null -> "";
            _ -> V
        end
    end, Row).

%% updates a channel record
update(Id, Fields) ->
    {Str, Vals} = entity:construct_kv_str(Fields),
    Statement = "UPDATE blob_store SET " ++ Str ++ " WHERE id=?",
    % de-atomize the status field if present
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = Statement,
        values    = [{id, Id}|Vals]
    }).