%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(poll_e).
-author("Yamka").
-license("MPL-2.0").
-description("The poll entity").

-include("entity.hrl").
-include("../packets/packet.hrl").
-include_lib("cqerl/include/cqerl.hrl").

-export([get/1, create/1]).
-export([vote/3, get_vote/2]).

%% gets a message state by ID
get(Id) ->
    {ok, Rows} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT * FROM polls WHERE id=?",
        values    = [{id, Id}]
    }),
    1 = cqerl:size(Rows),
    Row = #{options := Options} = maps:from_list(cqerl:head(Rows)),
    % option_votes maybe null if no one has voteed yet
    maps:map(fun(K, V) -> case K of
        option_votes when V =:= null ->
            [0 || _ <- Options];
        _ -> V
    end end, Row).

%% creates a message state
create(Options) ->
    Id = utils:gen_snowflake(),
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "INSERT INTO polls (id, options, total_votes) VALUES (?,?,0)",
        values = [{id, Id}, {options, Options}]
    }),
    Id.

vote(Id, User, Option) ->
    #{total_votes := TotalVotes, option_votes := OptionVotes} = poll_e:get(Id),
    if  Option > length(OptionVotes) -> {error, badopt};
        true ->
            {Before, [_|After]} = lists:split(Option),
            UpdOptionVotes = Before ++ [lists:nth(Option, OptionVotes) + 1] ++ After,
            {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
                statement = "BEGIN BATCH "
                    "UPDATE polls SET total_votes=?, option_votes=? WHERE id=?;"
                    "INSERT INTO poll_votes(poll, user, option) VALUES(?,?,?);"
                    "APPLY BATCH",
                values = [
                    {id, Id}, {total_votes, TotalVotes + 1}, {option_votes, UpdOptionVotes},
                    {poll, Id}, {user, User}, {option, Option}
                ]
            }),
            ok
    end.

get_vote(Id, User) ->
    {ok, Rows} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT option FROM poll_votes WHERE id=? AND user=?",
        values    = [{id, Id}, {user, User}]
    }),
    case cqerl:head(Rows) of
        empty_dataset -> {error, novote};
        [{option, Option}] -> {ok, Option}
    end.