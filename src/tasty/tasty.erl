-module(tasty).
-behaviour(gen_server).
-author("Order").
-license("MPL-2.0").
-description("The Tasty (voice protocol) gen_server").

-export([server_name/0]).
-export([create_session/3, get_session/1, register_user/3, unregister_user/1, broadcast/3]).
-export([start/0, stop/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {}).

%% TODO: buy a second server and make this function not static
server_name() -> "voice-hel1.ordermsg.tk".

create_session(K, U, C)  -> gen_server:call(?MODULE, {create_session, K, U, C}).
get_session(S)           -> gen_server:call(?MODULE, {get_session, S}).
register_user(S, Src, C) -> gen_server:cast(?MODULE, {register_user, S, Src, C}).
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
        [] -> nosession
    end, State};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast({register_user, I, Src, Co}, State) ->
    % ets:insert would replace the existing record
    [{I,K,U,C,_}] = ets:lookup(sessions, I),
    ets:insert(sessions, {I,K,U,C,Co}),
    ets:insert(srcs, {Src, I}),
    ets:insert(channel_users, {C, U, Co}),
    {noreply, State};

handle_cast({unregister_user, S}, State) ->
    [{S,_,U,C,Co}] = ets:lookup(sessions, S),
    ets:delete(sessions, S),
    ets:match_delete(srcs, {'_', S}),
    ets:match_delete(channel_users, {C, U, Co}),
    {noreply, State};

handle_cast({broadcast, Chan, Data, From}, State) ->
    % prefix: voice data (not video), user ID
    Prefixed = <<1, From:64/unsigned-integer, Data/binary>>,
    % broadcast data to controllers
    lists:foreach(fun({_, User, Controller}) ->
            % don't broadcast to sender
            if User =/= From ->
                Controller ! {broadcast, Prefixed};
               true -> ok
            end
        end, ets:lookup(channel_users, Chan)),
    {noreply, State};

handle_cast(_Msg, State)  -> {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.