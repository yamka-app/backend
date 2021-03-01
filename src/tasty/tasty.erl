-module(tasty).
-behaviour(gen_server).
-author("Order").
-license("MPL-2.0").
-description("The Tasty (voice protocol) gen_server").

-export([create_session/3, get_session/1]).
-export([start/0, stop/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {}).

create_session(RP, U, C) -> gen_server:call(?MODULE, {create_session, RP, U, C}).
get_session(S)           -> gen_server:call(?MODULE, {get_session, S}).
get_socket()             -> gen_server:call(?MODULE, get_socket).

stop(Name)       -> gen_server:call(Name, stop).
start()          -> start_link(?MODULE).
start_link(Name) -> gen_server:start_link({local, Name}, ?MODULE, [], []).
init(_Args) ->
    ets:new(sessions, [set, private, named_table]),
    ets:new(srcs, [set, private, named_table]),
    logging:log("Tasty gen_server running (node ~p)", [node()]),
    tasty_listener:start(),
    {ok, #state{}}.

handle_cast(_Msg, State)  -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
handle_call(stop, _From, State) -> {stop, normal, stopped, State};
handle_call({create_session, ReceivePub, User, Channel}, _, State) ->
    {SendPriv, SendPub} = crypto:generate_key(rsa, [65537, 2048]),
    SessionId = crypto:strong_rand_bytes(64),
    ets:insert(sessions, {SessionId, ReceivePub, SendPriv, User, Channel, _Controller=none}),
    {reply, {SessionId, SendPub}, State};

handle_call({get_session, Src={_,_}}, From, State) ->
    [{Src, Id}] = ets:lookup(Src),
    handle_call({get_session, Id}, From, State);

handle_call({get_session, Id}, _, State) ->
    [Session={Id,_,_,_,_,_}] = ets:lookup(sessions, Id),
    {reply, Session, State};

handle_call(_Request, _From, State) -> {reply, ok, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
