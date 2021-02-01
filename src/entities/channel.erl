-module(channel).
-author("Order").
-license("MPL-2.0").
-description("The channel entity").

-include("entity.hrl").
-include("../packets/packet.hrl").
-include_lib("cqerl/include/cqerl.hrl").

-export([get/1, create/2, create/4]).

%% gets a channel by ID
get(Id) ->
    {ok, Rows} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT * FROM channels WHERE id=?",
        values    = [{id, Id}]
    }),
    1 = cqerl:size(Rows),
    maps:from_list(cqerl:head(Rows)).

%% creates a channel
create(wall, Name)                 -> create(1, Name, 0, []).
create(normal, Name, Group, Perms) -> create(0, Name, Group, Perms);
create(Type, Name, Group, Perms) when is_integer(Type) ->
    Id = utils:gen_snowflake(),
    % execute the CQL query
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "INSERT INTO channels (id, name, group, lcid, type, perms VALUES (?,?,?,0,?,?)",
        values = [
            {id,    Id},
            {name,  Name},
            {group, Group},
            {type,  Type},
            {perms, Perms}
        ]
    }),
    Id.