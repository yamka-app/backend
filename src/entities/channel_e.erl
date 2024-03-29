%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(channel_e).
-author("Yamka").
-license("MPL-2.0").
-description("The channel entity").

-include_lib("cqerl/include/cqerl.hrl").

-export([get/1, create/4, update/2, delete/1]).
-export([get_dm/1, get_messages/4]).
-export([set_unread/3, get_unread/2]).
-export([add_mention/3, forget_mentions/2, get_mentions/2]).

%% gets a channel by ID
get(Id) ->
    {ok, Rows} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT * FROM channels WHERE id=?",
        values    = [{id, Id}]
    }),
    1 = cqerl:size(Rows),
    Voice = tasty:get_users_and_states(Id),
    maps:map(fun(K, V) ->
        case K of
            voice when V =:= null -> false; % the new "voice" field is null by default
            _ -> V
        end
    end, maps:merge(maps:from_list(cqerl:head(Rows)),
        #{voice_users  => [User   || {User, _}   <- Voice],
          voice_status => [Status || {_, Status} <- Voice]})).

%% creates a channel
create(Name, Group, Perms, Voice) ->
    Id = utils:gen_snowflake(),
    % execute the CQL query
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "INSERT INTO channels (id, name, group, lcid, perms, voice) VALUES (?,?,?,0,?,?)",
        values = [
            {id,    Id},
            {name,  Name},
            {group, Group},
            {perms, Perms},
            {voice, Voice}
        ]
    }),
    Id.

%% sets {UnreadLcid, FirstUnreadId}
set_unread(Id, User, {Lcid, Msg}) ->
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "UPDATE unread SET lcid=?, msg=? WHERE channel=? and user=?",
        values    = [{channel, Id}, {user, User}, {lcid, Lcid}, {msg, Msg}]
    }).

%% gets {UnreadLcid, FirstUnreadId} 
get_unread(Id, User) ->
    {ok, Rows} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT lcid, msg FROM unread WHERE channel=? and user=?",
        values    = [{channel, Id}, {user, User}]
    }),
    case cqerl:head(Rows) of
        [{lcid, Lcid}, {msg, 0}] ->
            case get_messages(Id, 0, 1, up) of
                [FirstMsg] -> {Lcid, FirstMsg};
                [] -> {Lcid, 0}
            end;
        [{lcid, Lcid}, {msg, Msg}] -> {Lcid, Msg};
        empty_dataset ->
            Lcid = maps:get(lcid, channel_e:get(Id)),
            [Msg|_] = if Lcid==0->[0];true->get_messages(Id, 9223372036854775807, 1, down)end, % gets the last message
            set_unread(Id, User, {Lcid, Msg}),
            {Lcid, Msg}
    end.

%% adds mention
add_mention(Id, User, Msg) ->
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "INSERT INTO mentions (channel, user, msg) VALUES (?,?,?)",
        values    = [{channel, Id}, {user, User}, {msg, Msg}]
    }).

%% forgets mentions before msg
forget_mentions(Id, User) ->
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "DELETE FROM mentions WHERE channel=? AND user=?",
        values    = [{channel, Id}, {user, User}]
    }).

%% gets mentions
get_mentions(Id, User) ->
    {ok, Rows} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT msg FROM mentions WHERE channel=? AND user=?",
        values    = [{channel, Id}, {user, User}]
    }),
    [MId || [{msg, MId}] <- cqerl:all_rows(Rows)].

%% gets messages IDs (with pagination)
get_messages(Id, StartId, Limit, down) -> get_messages(Id, StartId, Limit, "messages_id_by_chan", "<");
get_messages(Id, StartId, Limit, up)   -> get_messages(Id, StartId, Limit, "messages_id_by_chan_rev", ">").
get_messages(Id, StartId, Limit, Tab, Operator) ->
    {ok, Rows} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT * FROM " ++ Tab ++ " WHERE channel=? AND id" ++ Operator ++ "? LIMIT ? ALLOW FILTERING",
        values    = [{channel, Id}, {id, StartId}, {'[limit]', Limit}]
    }),
    [MId || [{channel, _}, {id, MId}] <- cqerl:all_rows(Rows)].

%% gets a DM channel two users share
get_dm([_,_]=Users) ->
    {ok, Rows} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT channel FROM dm_channels WHERE users=?",
        values    = [{users, Users}]
    }),
    case cqerl:head(Rows) of
        empty_dataset   -> nodm;
        [{channel, Id}] -> Id
    end.

%% updates a channel record
update(Id, Fields) ->
    {Str, Vals} = entity:construct_kv_str(Fields),
    Statement = "UPDATE channels SET " ++ Str ++ " WHERE id=?",
    % de-atomize the status field if present
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = Statement,
        values    = [{id, Id}|Vals]
    }).

%% deletes a channel
delete(Id) ->
    Statement = "DELETE FROM channels WHERE id=?",
    % de-atomize the status field if present
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = Statement,
        values    = [{id, Id}]
    }).