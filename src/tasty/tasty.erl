%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(tasty).
-behaviour(gen_server).
-author("Yamka").
-license("MPL-2.0").
-description("The Tasty (voice protocol) gen_server").

-include("../entities/entity.hrl").

-export([server_name/0]).
-export([create_session/3, get_users_and_states/1, get_session/1, register_user/3, unregister_user/1, broadcast/3]).
-export([add_speaking_flag/1, rm_speaking_flag/1]).
-export([start/0, stop/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {}).

%% TODO: buy a second server and make this function not static
server_name() -> "hel1.yamka.app".

create_session(K, U, C)  -> gen_server:call(?MODULE, {create_session, K, U, C}).
get_users_and_states(C)  -> gen_server:call(?MODULE, {get_users_and_states, C}).
get_session(S)           -> gen_server:call(?MODULE, {get_session, S}).
register_user(S, Src, C) -> gen_server:cast(?MODULE, {register_user, S, Src, C}).
unregister_user(S)       -> gen_server:cast(?MODULE, {unregister_user, S}).
broadcast(C, D, U)       -> gen_server:cast(?MODULE, {broadcast, C, D, U}).
add_speaking_flag(I)     -> gen_server:cast(?MODULE, {add_speaking_flag, I}).
rm_speaking_flag(I)      -> gen_server:cast(?MODULE, {rm_speaking_flag, I}).

stop(Name)       -> gen_server:call(Name, stop).
start()          -> start_link(?MODULE).
start_link(Name) -> gen_server:start_link({local, Name}, ?MODULE, [], []).
init(_Args) ->
    ets:new(sessions,      [set, private, named_table]),
    ets:new(srcs,          [set, private, named_table]),
    ets:new(channel_users, [bag, private, named_table]),
    logging:log("Tasty gen_server running (node ~p)", [node()]),
    {ok, #state{}}.

broadcast_connected(Channel) ->
    States = [{User, UserState} || {_, User, UserState, _} <- ets:lookup(channel_users, Channel)],
    Sorted = lists:sort(fun({A, _}, {B, _}) -> A =< B end, States),
    {Users, Statuses} = lists:unzip(Sorted),
    client:icpc_broadcast_to_aware(chan_awareness,
        #entity{type=channel, fields=#{
            id => Channel,
            voice_users  => Users,
            voice_status => Statuses
        }}, [id, voice_users, voice_status]).

handle_info(_Info, State) -> {noreply, State}.
handle_call(stop, _From, State) -> {stop, normal, stopped, State};
handle_call({create_session, Key, User, Channel}, _, State) ->
    SessionId = crypto:strong_rand_bytes(16),
    ets:insert(sessions, {SessionId, Key, User, Channel, _Controller=none}),
    {reply, SessionId, State};

handle_call({get_session, Src={_,_}}, From, State) ->
    case ets:lookup(srcs, Src) of
        [{Src, Id}] -> handle_call({get_session, Id}, From, State);
        [] -> {reply, nosession, State}
    end;

handle_call({get_session, <<Id/binary>>}, _, State) ->
    {reply, case ets:lookup(sessions, Id) of
        [Session={Id,_,_,_,_}] -> Session;
        [] -> badsession
    end, State};

handle_call({get_users_and_states, Channel}, _, State) ->
    {reply,
        [{User, UserState} || {_, User, UserState, _}
            <- ets:lookup(channel_users, Channel)],
        State};

handle_call(_Request, _From, State) -> {reply, badrq, State}.

handle_cast({register_user, I, Src, Co}, State) ->
    % ets:insert will replace the existing record
    [{I,K,U,C,_}] = ets:lookup(sessions, I),
    ets:insert(sessions, {I,K,U,C,Co}),
    ets:insert(srcs, {Src, I}),
    ets:insert(channel_users, {C, U, [], Co}),
    broadcast_connected(C),
    {noreply, State};

handle_cast({unregister_user, S}, State) ->
    case ets:lookup(sessions, S) of
        [{S,_,U,C,Co}] ->
            ets:delete(sessions, S),
            ets:match_delete(srcs, {'_', S}),
            ets:match_delete(channel_users, {C, U, '_', Co}),
            broadcast_connected(C);
        [] -> ok
    end,
    {noreply, State};

handle_cast({broadcast, Chan, Data, From}, State) ->
    % prefix: voice data (not video), user ID
    Prefixed = <<1, From:64/unsigned, Data/binary>>,
    % broadcast data to controllers
    lists:foreach(fun({_, User, _, Controller}) ->
            % don't broadcast to sender
            if User =/= From ->
                Controller ! {broadcast, Prefixed};
               true -> ok
            end
        end, ets:lookup(channel_users, Chan)),
    {noreply, State};

handle_cast({add_speaking_flag, S}, GenServerState) ->
    case ets:lookup(sessions, S) of
        [{S, _, U, C, _}] ->
            [Old={C, U, State, Co}] = ets:match_object(channel_users, {C, U, '_', '_'}),
            AlreadySpeaking = lists:member(speaking, State),
            if AlreadySpeaking -> ok;
            true ->
                    ets:match_delete(channel_users, Old),
                    ets:insert(channel_users, {C, U, [speaking|State], Co}),
                    broadcast_connected(C)
            end;
        [] -> ok
    end,
    {noreply, GenServerState};

handle_cast({rm_speaking_flag, S}, GenServerState) ->
    case ets:lookup(sessions, S) of
        [{S, _, U, C, _}] ->
            [Old={C, U, State, Co}] = ets:match_object(channel_users, {C, U, '_', '_'}),
            Speaking = lists:member(speaking, State),
            if not Speaking -> ok;
                true ->
                    ets:match_delete(channel_users, Old),
                    ets:insert(channel_users, {C, U, lists:delete(speaking, State), Co}),
                    broadcast_connected(C)
            end;
        [] -> ok
    end,
    {noreply, GenServerState};

handle_cast(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.