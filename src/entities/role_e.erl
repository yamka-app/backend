%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(role_e).
-author("Yamka").
-license("MPL-2.0").
-description("The role entity").

-include("./entity.hrl").
-include_lib("cqerl/include/cqerl.hrl").

-export([get/1, create/5, delete/1, nuke/1, nuke/2]).
-export([get_members/4, get_roles/2, add/2, remove/2]).
-export([perm_waterfall/1, perm_waterfall/2, perm_val/2, perm_set/3, perm_has/2, perm_atom/1]).

get(Id) ->
    {ok, Rows} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT * FROM roles WHERE id=?",
        values    = [{id, Id}]
    }),
    1 = cqerl:size(Rows),
    maps:map(fun(K, V) -> case K of
        permissions -> conformant_perms(V);
        _ -> V
    end end, maps:from_list(cqerl:head(Rows))).

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
        statement = "BEGIN BATCH;"
            "INSERT INTO roles_by_user (group, user, role) VALUES (?,?,?);"
            "INSERT INTO users_by_role (user, role) VALUES (?,?);"
            "APPLY BATCH",
        values = [{group, Group}, {user, User}, {role, Id}]
    }).

remove(Id, User) ->
    #{group := Group} = role_e:get(Id),
    remove(Id, Group, User).
remove(Id, Group, User) ->
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "BEGIN BATCH;"
            "DELETE FROM roles_by_user WHERE group=? AND role=? AND user=?;"
            "DELETE FROM users_by_role WHERE role=? AND user=?;"
            "APPLY BATCH",
        values = [{group, Group}, {user, User}, {role, Id}]
    }).

get_members(_Id, _StartId, _Limit, down) -> not_implemented;
get_members(Id, StartId, Limit, up)      -> get_members_worker(Id, StartId, Limit, "users_by_role", ">").
get_members_worker(Id, StartId, Limit, Tab, Operator) ->
    {ok, Rows} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT * FROM " ++ Tab ++ " WHERE role=? AND user" ++ Operator ++ "? LIMIT ? ALLOW FILTERING",
        values    = [{role, Id}, {user, StartId}, {'[limit]', Limit}]
    }),
    [MId || [{role, _}, {user, MId}] <- cqerl:all_rows(Rows)].

get_roles(User, Group) ->
    {ok, Rows} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT role FROM roles_by_user WHERE group=? AND user=?",
        values = [{group, Group}, {user, User}]
    }),
    [Role || [{role, Role}] <- cqerl:all_rows(Rows)].



perm_bit(Perm) -> maps:get(Perm, utils:swap_map(?ROLE_PERMISSION_BITS)).
perm_atom(Bit) -> maps:get(Bit, ?ROLE_PERMISSION_BITS).
perm_flag_val(Flag) -> maps:get(Flag, ?PERMISSION_FLAGS).
perm_val_flag(Val)  -> maps:get(Val, utils:swap_map(?PERMISSION_FLAGS)).

conformant_perms(<<Perms/bitstring>>) ->
    Trailing = ?PERM_LEN - bit_size(Perms),
    <<Perms/bitstring, 0:Trailing/unsigned-integer>>.
perm_val(<<Perms/bitstring>>, Which) when is_atom(Which) -> perm_val(Perms, perm_bit(Which));
perm_val(<<Perms/bitstring>>, Which) ->
    Offset = Which * 2,
    <<_:Offset/bitstring, Flag:2/bitstring, _/bitstring>> = Perms,
    perm_flag_val(Flag).
perm_set(<<Perms/bitstring>>, Which, What) when is_atom(Which) -> perm_set(Perms, perm_bit(Which), What);
perm_set(<<Perms/bitstring>>, Which, What) ->
    Offset = Which * 2,
    <<Before:Offset/bitstring, _:2/bitstring, After/bitstring>> = Perms,
    <<Before/bitstring, (perm_val_flag(What)):2/bitstring, After/bitstring>>.

perm_all(Val) -> perm_all(perm_val_flag(Val), ?PERM_LEN).
perm_all(_, 0) -> <<>>;
perm_all(Flag, Offs) -> <<Flag:2/bitstring, (perm_all(Flag, Offs - 2))/bitstring>>.

perm_combine(unset, B) -> B;
perm_combine(yes,   _) -> yes;
perm_combine(no,    _) -> no.

perm_combine_sets(A, B) -> perm_combine_sets(A, B, 0).
perm_combine_sets(_, _, Bit) when Bit =:= ?PERM_LEN div 2 -> <<>>;
perm_combine_sets(A, B, Bit) ->
    ValA = perm_val(A, Bit),
    ValB = perm_val(B, Bit),
    <<(perm_val_flag(perm_combine(ValA, ValB))):2/bitstring,
      (perm_combine_sets(A, B, Bit + 1))/bitstring>>.

perm_waterfall([])    -> perm_all(no);
perm_waterfall([A|T]) -> perm_combine_sets(A, perm_waterfall(T)).

order(Roles) ->
    lists:sort(fun(#{priority := A}, #{priority := B}) -> A =< B end, Roles).

perm_waterfall(User, Group) ->
    RoleIds = role_e:get_roles(User, Group),
    Roles = order([role_e:get(Id) || Id <- RoleIds]),
    perm_waterfall([Perms || #{permissions := Perms} <- Roles]).

perm_has(Set, Perm) -> perm_val(Set, Perm) =:= yes.