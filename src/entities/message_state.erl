-module(message_state).
-author("Order").
-license("MPL-2.0").
-description("The message state entity").

-include("entity.hrl").
-include("../packets/packet.hrl").
-include_lib("cqerl/include/cqerl.hrl").

-export([get/1, create/2]).

%% gets a message state by ID
get(Id) ->
    {ok, Rows} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT * FROM message_states WHERE id=?",
        values    = [{id, Id}]
    }),
    1 = cqerl:size(Rows),
    maps:map(fun(K, V) ->
        case K of
            sections -> [#message_section{type=maps:get(Type, ?MESSAGE_SECTION_TYPE_MAP), text=Te, blob=B}
                || [{type, Type}, {txt, Te}, {blob, B}] <- V];
            _ -> V
        end
    end, maps:from_list(cqerl:head(Rows))).

%% creates a message state
create(MsgId, Sections) ->
    Id = utils:gen_snowflake(),
    % execute the CQL query
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "INSERT INTO message_states (id, msg_id, sections) VALUES (?,?,?)",
        values = [
            {id,       Id},
            {msg_id,   MsgId},
            {sections, [
                [
                    {type, maps:get(Type, ?REVERSE_MESSAGE_SECTION_TYPE_MAP)},
                    {txt,  Text},
                    {blob, Blob}
                ] || #message_section{type=Type, text=Text, blob=Blob} <- Sections]}
        ]
    }),
    Id.