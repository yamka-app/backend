%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(email).
-behaviour(gen_server).
-author("Yamka").
-license("MPL-2.0").
-description("The email gen_server").

-define(EMAIL_REGEX, "(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?|[a-z0-9-]*[a-z0-9]:(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)\\])").

-export([is_valid/1]).
-export([send_confirmation/2]).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {password :: binary(), confirmation_template :: binary()}).
send_confirmation(To, Code) -> gen_server:cast(?MODULE, {send_confirmation, To, Code}).

start_link() -> gen_server:start_link({local, email}, ?MODULE, [], []).
init(_Args) ->
    lager:info("Email gen_server running (node ~p)", [node()]),
    {ok, Confirm} = file:read_file("/etc/yamka/email_templates/email_confirmation.html"),
    {ok, Password} = file:read_file("/run/secrets/smtp_pass"),
    {ok, #state{password=Password, confirmation_template=Confirm}}.

handle_info(_Info, State) -> {noreply, State}.

handle_call(_Request, _From, State) -> {reply, badrq, State}.

handle_cast({send_confirmation, To, Code}, State=#state{password=Pass, confirmation_template=Template}) ->
    Formatted = lists:flatten(io_lib:format(Template, [Code])),
    send_html({Formatted, "Confirm your email address"}, To, Pass),
    {noreply, State};

handle_cast(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

send_html({Text, Subj}, To, Pass) ->
    Relay = yamka_config:get(email_relay),
    gen_smtp_client:send({"noreply@" ++ Relay, [To],
        "Subject: " ++ Subj ++ "\r\n"
        "Content-Type: text/html;\r\n"
        "\tcharset=UTF-8\r\n\r\n"
        ++ unicode:characters_to_binary(Text)},
        [{relay, yamka_config:get(email_relay)},
         {username, "noreply"},
         {password, Pass},
         {tls, always}]).

is_valid(Email) ->
    {ok, Re} = re:compile(?EMAIL_REGEX, [caseless]),
    re:run(Email, Re) =:= {match, [{0, length(Email)}]}.