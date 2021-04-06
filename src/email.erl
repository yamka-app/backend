%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(email).
-behaviour(gen_server).
-author("Yamka").
-license("MPL-2.0").
-description("The email gen_server").

-define(RELAY, "mail.yamka.app").

-export([send_confirmation/2, send_emergency/4]).
-export([start/0, stop/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {password :: binary(), confirmation_template :: binary(), emergency_template :: binary()}).
send_confirmation(To, Code)                    -> gen_server:cast(?MODULE, {send_confirmation, To, Code}).
send_emergency   (To, User, Message, Location) -> gen_server:cast(?MODULE, {send_emergency, To, User, Message, Location}).

stop(Name)       -> gen_server:call(Name, stop).
start()          -> start_link(?MODULE).
start_link(Name) -> gen_server:start_link({local, Name}, ?MODULE, [], []).
init(_Args) ->
    logging:log("Email gen_server running (node ~p)", [node()]),
    {ok, Confirm}   = file:read_file("email/email_confirmation.html"),
    {ok, Emergency} = file:read_file("email/emergency.html"),
    {ok, Password}  = file:read_file("/run/secrets/smtp_pass"),
    {ok, #state{password=Password, confirmation_template=Confirm, emergency_template=Emergency}}.

handle_info(_Info, State) -> {noreply, State}.

handle_call(_Request, _From, State) -> {reply, badrq, State}.

handle_cast({send_confirmation, To, Code}, State=#state{password=Pass, confirmation_template=Template}) ->
    Formatted = lists:flatten(io_lib:format(Template, [Code])),
    send_html(Formatted, To, Pass),
    {noreply, State};

handle_cast({send_emergency, To, User, Message, Location}, State=#state{password=Pass, emergency_template=Template}) ->
    Formatted = lists:flatten(io_lib:format(Template, [User, Message, Location])),
    send_html(Formatted, To, Pass),
    {noreply, State};

handle_cast(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

send_html(Text, To, Pass) ->
    gen_smtp_client:send({"noreply@" ++ ?RELAY, [To],
        "Subject: Confirm your E-Mail address\r\n"
        "Content-Type: text/html;\r\n"
        "\tcharset=UTF-8\r\n\r\n"
        ++ unicode:characters_to_binary(Text)},
        [{relay, ?RELAY},
         {username, "noreply"},
         {password, Pass},
         {tls, always}]).