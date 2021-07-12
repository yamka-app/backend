%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(group_e).
-author("Yamka").
-license("MPL-2.0").
-description("The group entity").

-include_lib("cqerl/include/cqerl.hrl").

-export([get_channels/1, get_roles/1, get/1, get/2, create/2, delete/1]).
-export([get_invites/1, add_invite/1, remove_invite/2, resolve_invite/1]).
-export([find_users/3, cache_user_name/3]).
-export([assert_permission/3, has_permission/2]).

get_channels(Id) ->
    {ok, Rows} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT id FROM channels_by_group WHERE group=?",
        values    = [{group, Id}]
    }),
    [C || [{id, C}] <- cqerl:all_rows(Rows)].

get_roles(Id) ->
    {ok, Rows} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT id FROM roles_by_group WHERE group=?",
        values    = [{group, Id}]
    }),
    [C || [{id, C}] <- cqerl:all_rows(Rows)].
get_invites(Id) ->
    {ok, Rows} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT code FROM invites_by_group WHERE group=?",
        values    = [{group, Id}]
    }),
    [C || [{code, C}] <- cqerl:all_rows(Rows)].

is_field_public(id)   -> true;
is_field_public(name) -> true;
is_field_public(icon) -> true;
is_field_public(_)    -> false.

get(Id) -> get(Id, true).
get(Id, IncludeExtra) ->
    {ok, Rows} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT * FROM groups WHERE id=?",
        values    = [{id, Id}]
    }),
    1 = cqerl:size(Rows),
    Map = maps:from_list(cqerl:head(Rows)),
    if
        not IncludeExtra ->
            maps:filter(fun(K, _) -> is_field_public(K) end, Map);
        true ->
            maps:merge(Map, #{
                channels => get_channels(Id),
                roles    => get_roles(Id),
                invites  => get_invites(Id)})
    end.

create(Name, Owner) ->
    Id = utils:gen_snowflake(),
    Everyone = role_e:create(Id, "everyone", 0, 1, 0),
    Icon = file_storage:register_file(utils:gen_avatar(), "group_icon.png"),
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "INSERT INTO groups (id, name, icon, owner, everyone_role) VALUES (?,?,?,?,?)",
        values = [
            {id, Id}, {icon, Icon}, {name, Name}, {owner, Owner},
            {everyone_role, Everyone}
        ]
    }),
    % create a default channel
    channel_e:create("Text 1", Id, [], false),
    {Id, Everyone}.

delete(Id) ->
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "DELETE FROM groups WHERE id=?",
        values = [{id, Id}]
    }).

add_invite(Id) ->
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "INSERT INTO invites (group, code) VALUES (?,?)",
        values    = [{group, Id}, {code, utils:gen_invite()}]
    }).
remove_invite(_, Code) ->
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "DELETE FROM invites WHERE code=?",
        values    = [{code, Code}]
    }).
resolve_invite(Code) ->
    {ok, Rows} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT group FROM invites WHERE code=?",
        values    = [{code, Code}]
    }),
    case cqerl:head(Rows) of
        empty_dataset -> error;
        [{group, Id}] -> {ok, Id}
    end.

find_users(Id, Name, Max) when is_list(Name) ->
    find_users(Id, unicode:characters_to_binary(Name), Max);
find_users(Id, Name, Max) when Max > 5 ->
    find_users(Id, Name, 5);
find_users(Id, Name, Max) ->
    IdStr = integer_to_binary(Id),
    % don't be afraid, come here...
    % it's not actually that complicated,
    % we ask Elasticsearch (or rather Elassandra in our case)
    % to find user IDs that belong to the group with ID `Id`
    % and whose names start with `Name`
    %
    % the problem is that Elastic, despite being attached
    % to Cassandra in our case, stores data separately from
    % the main DB, so we have to make sure our past selves cache it
    {ok, Response} = erlastic_search:search(<<"usernames">>, <<"group_local">>,
      [{<<"query">>,
        [{<<"bool">>, [
          {<<"should">>, [
           [{<<"term">>, [{<<"group">>, IdStr}]}],
           [{<<"query_string">>, [{<<"query">>, <<Name/binary, "*">>}]}]]},
          {<<"minimum_should_match">>, 2}]}]},
       {<<"size">>, Max}]),
    
    Hits = proplists:get_value(<<"hits">>, proplists:get_value(<<"hits">>, Response)),
    [binary_to_integer(proplists:get_value(<<"_id">>, Hit)) || Hit <- Hits].

cache_user_name(Id, User, Name) when is_list(Name) ->
    cache_user_name(Id, User, unicode:characters_to_binary(Name));
cache_user_name(Id, User, Name) ->
    % Elassandra uses "upsert" operations for indexations,
    % so we don't need to explicitly update/delete anything,
    % just provide a new document with the same ID
    erlastic_search:index_doc_with_id(<<"usernames">>, <<"group_local">>,
      integer_to_binary(User),
      [{<<"name">>, Name},
       {<<"group">>, integer_to_binary(Id)}]).

has_permission(Id, Perm) ->
    % group owners have every permission regardless of their roles
    #{owner := Owner} = group_e:get(Id),
    (erlang:get(id) =:= Owner)
    or
    role_e:perm_has(role_e:perm_waterfall(erlang:get(id), Id), Perm).

assert_permission(Id, Perm, {ScopeRef, Seq}) ->
    ok.
    % {_, true} = {{ScopeRef,
    %     status_packet:make(permission_denied, "Missing " ++ atom_to_list(Perm) ++ " group permission", Seq)},
    %         has_permission(Id, Perm)}.