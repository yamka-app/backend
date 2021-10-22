%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%%% TODO: make this a gen_server

-module(sweet_main).
-behaviour(gen_server).
-author("Yamka").
-license("MPL-2.0").
-description("Main server process for each client").

-include("../entities/entity.hrl").
-include("../packets/packet.hrl").

-record(state, {
    encoder :: pid() | nopid,
    socket :: ssl:socket(),
    cassandra,
    conn_state :: atom(),
    user_info :: tuple(),
    mfa_state :: term(),
    proto_ver :: number(),
    comp :: boolean(),
    seq :: number(),
    file_recv_pid :: pid() | nopid
}).

-export([start_link/2, fake_start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([switch_state/2, switch_state/3, send_packet/2, stop/1,
         ratelimit/2, ratelimit/3, get_state/1,
         route_to_aware/2, route_to_aware/3,
         route_to_owners/2, route_to_owners/3,
         get_file_recv_pid/1, set_file_recv_pid/2,
         get_user_info/1,
         transmit/2, packet/2]).

fake_start_link(Pid) -> {ok, Pid}.
start_link(S, C) -> gen_server:start_link(?MODULE, [S, C], []).
%% starts a main process
init([Socket, Cassandra]) ->
    % create rate limiters
    ratelimit:make(packet,     {100, 1000  }),
    ratelimit:make(close,      {105, 1000  }),
    ratelimit:make(login,      {5,   30000 }),
    ratelimit:make(entity,     {300, 1000  }),
    ratelimit:make(message,    {20,  10000 }),
    ratelimit:make(bot_create, {1,   120000}),
    ratelimit:make(voice,      {2,   1000  }),

    {ok, #state{
        socket = Socket,
        cassandra = Cassandra,
        conn_state = awaiting_login,
        user_info = {},
        mfa_state = {},
        proto_ver = 0,
        comp = false,
        seq = 0,
        file_recv_pid = nopid
    }}.

%%%
%%% gen_server callbacks
%%%

% handle packets received by the decoder
handle_call({packet, Packet=#packet{}}, _From, State=#state{user_info=UserInfo, cassandra=Cas}) ->
    lager:debug("--> ~p", [Packet]),
    Id = case UserInfo of
        {Val, _, _} -> Val;
        _ -> undefined
    end,
    spawn(sweet_handler, start, [self(), Id, Cas, Packet]),
    {reply, ok, State};

% switch the state (and possibly the protocol version) of the protocol processes when the handler asks for it
handle_call({switch_state, awaiting_mfa, NewMfa}, _From, State=#state{}) ->
    {reply, ok, State#state{conn_state = awaiting_mfa, mfa_state = NewMfa}};
handle_call({switch_state, normal, {Id,_,_}=NewUserInfo}, _From, State=#state{}) ->
    sweet_owners:add(Id, self()),
    {reply, ok, State#state{conn_state = normal, user_info = NewUserInfo}};

% send packets when asked by a packet handler or another main process
handle_call({transmit, Packet}, _From, State=#state{seq=Seq, encoder=Enc}) ->
    Seqd = Packet#packet{seq=Seq + 1}, % write seq
    lager:debug("<-- ~p", [Seqd]),
    Enc ! {packet, Seqd},
    {reply, ok, State#state{seq=Seq + 1}};

% route an entity to another main process when asked by a handler process of ours
handle_call({route, {aware, {_Type, _Id}=Spec}, Entity}, _From, State=#state{}) ->
    sweet_awareness:notify(Spec, Entity),
    {reply, ok, State};
handle_call({route, {owners, Id}, Entity}, _From, State=#state{}) ->
    sweet_owners:notify(Id, Entity),
    {reply, ok, State};

% updates awareness
handle_call({awareness, Action, {_Type, _Id}=Entity}, _From, State=#state{}) ->
    sweet_awareness:Action(Entity, self()), % apparently you can call functions specifying their name with a variable!
    {reply, ok, State};

% hits a rate limiter
handle_call({ratelimit, Name, Times}, _From, State=#state{}) ->
    {reply, ratelimit:hit(Name, Times), State};

% gets the connection state
handle_call(get_state, _From, State=#state{mfa_state=MfaState, conn_state=ConnState}) ->
    {reply, {ConnState, MfaState}, State};

% gets the file receiver PID
handle_call(get_recv, _From, State=#state{file_recv_pid=FileRecvPid}) ->
    {reply, FileRecvPid, State};

% sets the file receiver PID
handle_call({set_recv, Pid}, _From, State=#state{}) ->
    {reply, ok, State#state{file_recv_pid=Pid}};

% gets user info
handle_call(get_user_info, _From, State=#state{user_info=UserInfo}) ->
    {reply, UserInfo, State};

% shut down when asked
handle_call(stop, _From, State=#state{socket=Socket}) ->
    ssl:close(Socket),
    {stop, asked_to, ok, State}.

handle_cast(_, State=#state{}) -> {noreply, State}.

handle_info({encoder_pid, Enc}, State=#state{}) ->
    {noreply, State#state{encoder=Enc}};
handle_info(_, State) -> {noreply, State}.

%%%
%%% API
%%% 

%% switches the connection state

switch_state(Pid, awaiting_mfa, Mfa) -> gen_server:call(Pid, {switch_state, awaiting_mfa, Mfa});
switch_state(Pid, normal, UserInfo) -> gen_server:call(Pid, {switch_state, normal, UserInfo}).
switch_state(Pid, Target) -> gen_server:call(Pid, {switch_state, Target}).

%% sends a packet to the connected user
send_packet(Pid, P) -> gen_server:call(Pid, {transmit, P}).

%% routes an entity using the destination spec
route_entity(Pid, DestSpec, E=#entity{fields=F}, Allowed) ->
    route_entity(Pid, DestSpec, E#entity{fields=maps:filter(fun(K, _) ->
        lists:member(K, Allowed) end, F)}).
route_entity(Pid, DestSpec, E) -> gen_server:call(Pid, {route, DestSpec, E}).

%% routes an entity to users that have requested it before
route_to_aware(Pid, Entity={Type, Id}, Allowed) ->
    route_entity(Pid, {aware, Entity}, entity:get_record(Type, Id), Allowed).
route_to_aware(Pid, Entity={Type, Id}) ->
    route_entity(Pid, {aware, Entity}, entity:get_record(Type, Id)).

%% routes a user entity to the owners (there might be multiple devices the user is logged in from)
route_to_owners(Pid, Id, Allowed) ->
    route_entity(Pid, {owners, Id}, entity:get_record(user, Id), Allowed).
route_to_owners(Pid, Id) ->
    route_entity(Pid, {owners, Id}, entity:get_record(user, Id)).

%% hits a rate limiter
ratelimit(Pid, Name) ->
    ratelimit(Pid, Name, 1).
ratelimit(Pid, Name, Times) ->
    gen_server:call(Pid, {ratelimit, Name, Times}).

%% gets the connection state
get_state(Pid) -> gen_server:call(Pid, get_state).

%% gets the file receiver PID
get_file_recv_pid(Pid) -> gen_server:call(Pid, get_recv).

%% gets the file receiver PID
get_user_info(Pid) -> gen_server:call(Pid, get_user_info).

%% sets the file receiver PID
set_file_recv_pid(Pid, Recv) -> gen_server:call(Pid, {set_recv, Recv}).

%% transmits a packet
transmit(Pid, Packet) -> gen_server:call(Pid, {transmit, Packet}).

%% processes a packet
packet(Pid, Packet) -> gen_server:call(Pid, {packet, Packet}).

%% disconnects the client
stop(Pid) -> gen_server:call(Pid, stop).