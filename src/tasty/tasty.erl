-module(tasty).
-behaviour(gen_server).
-author("Order").
-license("MPL-2.0").
-description("The Tasty (voice protocol) gen_server").

-export([create_session/3, get_session/1, register_user/2, unregister_user/1, broadcast/3]).
-export([start/0, stop/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {}).

create_session(RP, U, C) -> gen_server:call(?MODULE, {create_session, RP, U, C}).
get_session(S)           -> gen_server:call(?MODULE, {get_session, S}).
register_user(S, C)      -> gen_server:cast(?MODULE, {register_user, S, C}).
unregister_user(S)       -> gen_server:cast(?MODULE, {unregister_user, S}).
broadcast(C, D, U)       -> gen_server:cast(?MODULE, {broadcast, C, D, U}).

stop(Name)       -> gen_server:call(Name, stop).
start()          -> start_link(?MODULE).
start_link(Name) -> gen_server:start_link({local, Name}, ?MODULE, [], []).
init(_Args) ->
    ets:new(sessions,      [set, private, named_table]),
    ets:new(srcs,          [set, private, named_table]),
    ets:new(channel_users, [set, private, named_table]),
    logging:log("Tasty gen_server running (node ~p)", [node()]),
    tasty_listener:start(),
    {ok, #state{}}.

handle_info(_Info, State) -> {noreply, State}.
handle_call(stop, _From, State) -> {stop, normal, stopped, State};
handle_call({create_session, Key, User, Channel}, _, State) ->
    SessionId = crypto:strong_rand_bytes(64),
    ets:insert(sessions, {SessionId, Key, User, Channel, _Controller=none}),
    {reply, SessionId, State};

handle_call({get_session, Src={_,_}}, From, State) ->
    [{Src, Id}] = ets:lookup(srcs, Src),
    handle_call({get_session, Id}, From, State);

handle_call({get_session, <<Id/binary>>}, _, State) ->
    [Session={Id,_,_,_,_}] = ets:lookup(sessions, Id),
    {reply, Session, State};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast({register_user, S, C}, State) ->
    % ets:insert would replace the existing record
    [{I,K,U,Co,_}] = ets:lookup(sessions, S),
    ets:insert(sessions, {I,K,U,C,Co}),
    ets:insert(channel_users, {C, U, Co}),
    {noreply, State};

handle_cast({unregister_user, S}, State) ->
    ets:delete(sessions, S),
    {noreply, State};

handle_cast({broadcast, Chan, Data, From}, State) ->
    % prefix: voice data (not video), user ID
    Prefixed = <<0, From:64/unsigned-integer, Data/binary>>,
    % broadcast data to controllers
    lists:foreach(fun({_, _, Controller}) ->
            Controller ! {broadcast, Prefixed}
        end, ets:lookup(channel_users, Chan)),
    {noreply, State};

handle_cast(_Msg, State)  -> {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.