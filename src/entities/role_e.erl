%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(role_e).
-author("Yamka").
-license("MPL-2.0").
-description("The role entity").

-include_lib("cqerl/include/cqerl.hrl").

-export([get/1, create/5, delete/1, nuke/1, nuke/2]).
-export([get_members/4, add/2, remove/2]).

get(Id) ->
    {ok, Rows} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT * FROM roles WHERE id=?",
        values    = [{id, Id}]
    }),
    1 = cqerl:size(Rows),
    maps:filter(fun(K,_)->K/=permissions end, maps:from_list(cqerl:head(Rows))).

create(Group, Name, Color, Priority, Perms) ->
    Id = utils:gen_snowflake(),
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "INSERT INTO roles (id, group, color, name, permissions, priority) VALUES (?,?,?,?,?,?)",
        values = [
            {id, Id}, {group, Group}, {color, Color}, {name, Name}, {permissions, <<Perms/unsigned-integer>>},
            {priority, Priority}
        ]
    }),
    Id.

delete(Id) ->
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "DELETE FROM roles WHERE id=?",
        values    = [{id, Id}]
    }).

%% deletes all member records in addition to the role itself
nuke(Id) -> nuke(Id, false).
nuke(Id, RemGroup) ->
    delete(Id),
    #{group := Group} = role_e:get(Id),
    nuke(Id, Group, 0, 1000, RemGroup).
nuke(Id, Group, From, Batch, RemGroup) ->
    Members = get_members(Id, From, Batch, up),
    lists:foreach(fun(User) ->
        remove(Id, Group, User),
        if RemGroup -> user_e:manage_contact(User, remove, {group, Group});
           true -> ok
        end
      end, Members),
    case length(Members) of
        Batch -> nuke(Id, Group, From, Batch, RemGroup);
        _     -> ok
    end.

add(Id, User) ->
    #{group := Group} = role_e:get(Id),
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "INSERT INTO roles_by_user (group, user, role) VALUES (?,?,?)",
        values = [{group, Group}, {user, User}, {role, Id}]
    }),
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "INSERT INTO users_by_role (user, role) VALUES (?,?)",
        values = [{user, User}, {role, Id}]
    }).

remove(Id, User) ->
    #{group := Group} = role_e:get(Id),
    remove(Id, Group, User).
remove(Id, Group, User) ->
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "DELETE FROM roles_by_user WHERE group=? AND role=? AND user=?",
        values = [{group, Group}, {user, User}, {role, Id}]
    }),
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "DELETE FROM users_by_role WHERE role=? AND user=?",
        values = [{user, User}, {role, Id}]
    }).

get_members(_Id, _StartId, _Limit, down) -> not_implemented;
get_members(Id, StartId, Limit, up)      -> get_members_worker(Id, StartId, Limit, "users_by_role", ">").
get_members_worker(Id, StartId, Limit, Tab, Operator) ->
    {ok, Rows} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT * FROM " ++ Tab ++ " WHERE role=? AND user" ++ Operator ++ "? LIMIT ? ALLOW FILTERING",
        values    = [{role, Id}, {user, StartId}, {'[limit]', Limit}]
    }),
    [MId || [{role, _}, {user, MId}] <- cqerl:all_rows(Rows)].