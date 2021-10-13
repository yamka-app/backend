%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(cassandra).
-author("Yamka").
-license("MPL-2.0").
-description("Cassandra abstraction module. Relies on a \"cassandra\" process dictionary key").

-include_lib("cqerl/include/cqerl.hrl").

-export([select/2, select/3]).
-export([select_one/2, select_one/3]).

select(Table, Where) ->
    select(Table, ["*"], Where).
select(Table, Columns, Where) ->
    WhereKeys = maps:keys(Where),
    {ok, Result} = cqerl:run_query(get(cassandra), #cql_query{
        statement = "SELECT " ++ lists:join(", ", Columns)
                ++ " FROM " ++ Table
                ++ " WHERE " ++ lists:join(" AND ", [atom_to_list(Key) ++ "=?" || Key <- WhereKeys]),
        values    = maps:to_list(Where)
    }),

    [maps:from_list(Proplist) || Proplist <- cqerl:all_rows(Result)].

select_one(Table, Where) ->
    select_one(Table, ["*"], Where).
select_one(Table, Columns, Where) ->
    Result = select(Table, Columns, Where),
    case Result of
        [] -> empty;
        [R] -> R;
        [_|_] -> more_than_one
    end.