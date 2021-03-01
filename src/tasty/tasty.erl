-module(tasty).
-behaviour(gen_server).
-author("Order").
-license("MPL-2.0").
-description("The Tasty (voice protocol) gen_server").

-export([start/0, stop/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {stbl=0}).

stop(Name)       -> gen_server:call(Name, stop).
start()          -> start_link(?MODULE).
start_link(Name) -> gen_server:start_link({local, Name}, ?MODULE, [], []).
init(_Args) ->
    Table = ets:new(sessions, [set, private, named_table]),
    logging:log("Tasty server running (node ~p)", [node()]),
    {ok, #state{stbl=Table}}.

handle_cast(_Msg, State)  -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
handle_call(stop, _From, State)     -> {stop, normal, stopped, State};
handle_call({create_session, ReceivePub, User, Channel}, _, State) ->
    Table = State#state.stbl,
    {SendPriv, SendPub} = crypto:generate_key(rsa, [65537, 2048]),
    SessionId = crypto:strong_rand_bytes(64),
    ets:insert(Table, {SessionId, ReceivePub, SendPriv, User, Channel}),
    {reply, {SessionId, SendPub}, State};

handle_call(_Request, _From, State) -> {reply, ok, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
