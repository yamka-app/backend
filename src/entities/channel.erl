-module(channel).
-author("Order").
-license("MPL-2.0").
-description("The channel entity").

-include("entity.hrl").
-include("../packets/packet.hrl").
-include_lib("cqerl/include/cqerl.hrl").

-export([get/1, get_dm/1, create/1, create/4, get_messages/4]).
-export([set_unread/3, get_unread/2]).

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
        [{lcid, Lcid}, {msg, Msg}] -> {Lcid, Msg};
        empty_dataset ->
            Lcid = maps:get(lcid, channel:get(Id)),
            [Msg|_] = if Lcid==0->[0];true->get_messages(Id, 16#FFFFFFFFFFFFFFFF, 1, down)end, % gets the last message
            set_unread(Id, User, {Lcid, Msg}),
            {Lcid, Msg}
    end.

%% gets messages IDs (with pagination)
get_messages(Id, StartId, Limit, down) -> get_messages_worker(Id, StartId, Limit, "message_ids_by_chan");
get_messages(Id, StartId, Limit, up)   -> get_messages_worker(Id, StartId, Limit, "message_ids_by_chan_reverse").
get_messages_worker(Id, StartId, Limit, Tab) ->
    {ok, Rows} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT * FROM " ++ Tab ++ " WHERE channel=? AND id>? LIMIT ? ALLOW FILTERING",
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