-module(channel).
-author("Order").
-license("MPL-2.0").
-description("The channel entity").

-include("entity.hrl").
-include("../packets/packet.hrl").
-include_lib("cqerl/include/cqerl.hrl").

-export([get/1, get_dm/1, create/1, create/4, update/2, get_messages/4]).
-export([set_unread/3, get_unread/2, reg_msg/2]).
-export([get_typing/1, set_typing/2, reset_typing/2]).

-define(TYPING_RESET_THRESHOLD, 15000).

%% gets a channel by ID
get(Id) ->
    {ok, Rows} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT * FROM channels WHERE id=?",
        values    = [{id, Id}]
    }),
    1 = cqerl:size(Rows),
    maps:map(fun(K, V) ->
        case K of
            type -> maps:get(V, ?CHANNEL_TYPE_MAP);
            _ -> V
        end
    end, maps:from_list(cqerl:head(Rows))).

%% creates a channel
create(wall)                       -> create(1, "Wall", 0, []).
create(normal, Name, Group, Perms) -> create(0, Name, Group, Perms);
create(Type, Name, Group, Perms) when is_integer(Type) ->
    Id = utils:gen_snowflake(),
    % execute the CQL query
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "INSERT INTO channels (id, name, group, lcid, type, perms) VALUES (?,?,?,0,?,?)",
        values = [
            {id,    Id},
            {name,  Name},
            {group, Group},
            {type,  Type},
            {perms, Perms}
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
            [FirstMsg|_] = get_messages(Id, 0, 1, up),
            {Lcid, FirstMsg};
        [{lcid, Lcid}, {msg, Msg}] -> {Lcid, Msg};
        empty_dataset ->
            Lcid = maps:get(lcid, channel:get(Id)),
            [Msg|_] = if Lcid==0->[0];true->get_messages(Id, 9223372036854775807, 1, down)end, % gets the last message
            set_unread(Id, User, {Lcid, Msg}),
            {Lcid, Msg}
    end.

%% gets messages IDs (with pagination)
get_messages(Id, StartId, Limit, down) -> get_messages_worker(Id, StartId, Limit, "message_ids_by_chan", "<");
get_messages(Id, StartId, Limit, up)   -> get_messages_worker(Id, StartId, Limit, "message_ids_by_chan_reverse", ">").
get_messages_worker(Id, StartId, Limit, Tab, Operator) ->
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
    [{channel, Id}] = cqerl:head(Rows),
    Id.

%% updates a channel record
update(Id, Fields) ->
    {Str, Vals} = entity:construct_kv_str(Fields),
    Statement = "UPDATE channels SET " ++ Str ++ " WHERE id=?",
    % de-atomize the status field if present
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = Statement,
        values    = [{id, Id}|Vals]
    }).

%% registers a message
reg_msg(Id, Msg) ->
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "INSERT INTO message_ids_by_chan (channel, id) VALUES (?,?)",
        values    = [{channel, Id}, {id, Msg}]
    }),
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "INSERT INTO message_ids_by_chan_reverse (channel, id) VALUES (?,?)",
        values    = [{channel, Id}, {id, Msg}]
    }).

%% gets the users who are typing
get_typing_filter([]) -> [];
get_typing_filter([{_, {User, Time}}|T]) ->
    MsSince = utils:ms_since(Time),
    if MsSince >= ?TYPING_RESET_THRESHOLD -> get_typing_filter(T);
       true -> [User|get_typing_filter(T)]
    end.
get_typing(Id) -> get_typing_filter(ets:lookup(typing, Id)).

%% adds a user to the typing list
set_typing(Id, User) -> ets:insert(typing, {Id, {User, erlang:monotonic_time()}}).
%% removes a user from the typing list
reset_typing(Id, User) -> ets:match_delete(typing, {Id, {User, '_'}}).