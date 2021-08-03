%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(message_state_e).
-author("Yamka").
-license("MPL-2.0").
-description("The message state entity").

-include("entity.hrl").
-include_lib("cqerl/include/cqerl.hrl").

-export([get/1, create/2]).
-export([filter_sections/1, parse_mentions/1]).

%% gets a message state by ID
get(Id) ->
    {ok, Rows} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT * FROM message_states WHERE id=?",
        values    = [{id, Id}]
    }),
    1 = cqerl:size(Rows),
    maps:filter(fun(_, V) -> V =/= null end, maps:map(fun(K, V) ->
        case K of
            sections when V =/= null -> [#message_section{type=maps:get(Type, ?MESSAGE_SECTION_TYPE_MAP), text=Te, blob=B}
                || [{type, Type}, {txt, Te}, {blob, B}] <- V];
            _ -> V
        end
    end, maps:from_list(cqerl:head(Rows)))).

%% creates a message state
create(MsgId, <<Encrypted/binary>>) ->
    Id = utils:gen_snowflake(),
    % execute the CQL query
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "INSERT INTO message_states (id, msg_id, encrypted) VALUES (?,?,?)",
        values = [{id, Id}, {msg_id, MsgId}, {encrypted, Encrypted}]
    }),
    Id;
create(MsgId, Sections) ->
    Id = utils:gen_snowflake(),
    % execute the CQL query
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "INSERT INTO message_states (id, msg_id, sections) VALUES (?,?,?)",
        values = [
            {id,     Id},
            {msg_id, MsgId},
            {sections, [
                [
                    {type, maps:get(Type, ?REVERSE_MESSAGE_SECTION_TYPE_MAP)},
                    {txt,  Text},
                    {blob, Blob}
                ] || #message_section{type=Type, text=Text, blob=Blob} <- Sections]}
        ]
    }),
    Id.

% limit text type sections to 4096 chars and strip them of whitespaces and newlines
filter_section({text, T, _})   -> #message_section{type=text,   text=utils:filter_text(T), blob=0};
% limit code sections to 4096 chars
filter_section({code, T, _})   -> #message_section{type=code,   text=string:slice(T, 0, 8192), blob=0};
% if the quote has no reference ID, it's text
% otherwise it's a reply
filter_section({quote, T, 0})  -> #message_section{type=quote,  text=utils:filter_text(T)};
filter_section({quote, _, Q})  -> #message_section{type=quote,  text="", blob=Q};
% filter out text in files and polls
filter_section({file, _, F})   -> #message_section{type=file,   text="", blob=F};
filter_section({poll, _, F})   -> #message_section{type=poll,   text="", blob=F};
% invites are no more than 12 chars long
filter_section({invite, I, _}) -> #message_section{type=invite, text=string:slice(I, 0, 12), blob=0};
% usernames are no more than 96 chars long
filter_section({user, I, _})   -> #message_section{type=user,   text=string:slice(I, 0, 96), blob=0};
% I don't really think your bot needs more than 8k characters for its UI
filter_section({bot_ui, I, _}) -> #message_section{type=bot_ui, text=string:slice(I, 0, 8192), blob=0}.

let_through({quote, Text, B}) -> (length(Text) > 0) or (B > 0);
let_through({T, _Text, B}) when T =:= file; T =:= poll -> B > 0;
let_through({_, Text, _B}) -> length(Text) > 0.

filter_sections([]) -> [];
filter_sections([#message_section{type=T, text=Te, blob=B}|Tail]) ->
    Filtered = #message_section{type=FT, text=FTe, blob=FB} = filter_section({T, Te, B}),
    Rest = filter_sections(Tail),
    Let = let_through({FT, FTe, FB}),
    if
        Let -> [Filtered|Rest];
        true -> Rest
    end.

parse_mention_matches(Str, [[{Idx, Len}]|Tail]) ->
    IdStr = string:slice(Str, Idx + 1, Len - 1),
    [list_to_integer(IdStr)|parse_mention_matches(Str, Tail)];
parse_mention_matches(_, []) -> [].

parse_mentions([#message_section{type=text, text=Text, blob=0}|Tail]) ->
    % a number preceded by an at sign but not a backslash
    ReResult = re:run(Text, "(?<!\\\\)@[0-9]+", [global, unicode]),
    case ReResult of
        nomatch          -> parse_mentions(Tail);
        {match, Matches} ->
            parse_mention_matches(Text, Matches) ++ parse_mentions(Tail)
    end;
parse_mentions([_|Tail]) -> parse_mentions(Tail);
parse_mentions([]) -> [].