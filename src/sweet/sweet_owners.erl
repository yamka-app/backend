%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%%% Owner storage. Keeps tracks of clients that are logged in as a
%%% specific user

-module(sweet_owners).
-behaviour(gen_server).
-author("Yamka").
-license("MPL-2.0").
-description("Owner storage (backed by Cassandra)").

-include("../entities/entity.hrl").
-include_lib("cqerl/include/cqerl.hrl").

-export([init/1, handle_call/3, handle_cast/2]).
-export([start_link/0, stop/0,
         add/2, remove/2, remove/1, notify/2, is_online/1]).

-record(state, {}).

%%% gen_server callbacks

init(_) ->
    ets:new(ownership, [bag, named_table]),
    {ok, #state{}}.

handle_call({add, Id, MainProcess}, _From, State) ->
    ets:insert(ownership, {Id, MainProcess}),
    lager:debug("~p is now an owner of ~p", [MainProcess, Id]),
    {reply, ok, State};

handle_call({remove, Id, MainProcess}, _From, State) ->
    ets:delete_object(ownership, {Id, MainProcess}),
    lager:debug("~p is no longer an owner of ~p", [MainProcess, Id]),
    {reply, ok, State};

handle_call({remove, MainProcess}, _From, State) ->
    ets:match_delete(ownership, {'_', MainProcess}),
    lager:debug("~p is no longer an owner of anything", [MainProcess]),
    {reply, ok, State};

handle_call({notify, Id, Entity}, _From, State) ->
    Nodes = [node() | nodes()],
    gen_server:abcast(Nodes, owner_server, {notify, Id, Entity}),
    lager:debug("broadcasted updates to owners across ~p nodes", [length(Nodes)]),
    {reply, ok, State};

handle_call({is_online, Id}, _From, State) ->
    Online = ets:lookup(ownership, Id),
    {reply, length(Online) >= 1, State}.

handle_cast({notify, Id, Entity}, State) ->
    Objects = ets:lookup(ownership, Id),
    Packet = entities_packet:make([Entity]),
    [sweet_main:send_packet(Pid, Packet) || {_, Pid} <- Objects],
    {noreply, State}.

%%% API

%% starts the server
-spec start_link() -> gen_server:start_link().
start_link() -> gen_server:start_link({local, owner_server}, ?MODULE, [], []).

%% stops the server
-spec stop() -> gen_server:stop().
stop() -> gen_server:stop(owner_server, ?MODULE, [], []).

%% Remembers that the specified MainProcess' client owns an account
-spec add(integer(), pid()) -> ok.
add(Id, MainProcess) -> gen_server:call(owner_server, {add, Id, MainProcess}).

%% Forgets that the specified MainProcess' client ows an account
-spec remove(integer(), pid()) -> ok.
remove(Id, MainProcess) -> gen_server:call(owner_server, {remove, Id, MainProcess}).

%% Forgets the specified MainProcess
-spec remove(pid()) -> ok.
remove(MainProcess) -> gen_server:call(owner_server, {remove, MainProcess}).

%% Notifies all owners about a user update
-spec notify(integer(), #entity{}) -> ok.
notify(Id, Entity) -> gen_server:call(owner_server, {notify, Id, Entity}).

%% Determines if a user is online
-spec is_online(integer()) -> boolean().
is_online(Id) -> gen_server:call(owner_server, {is_online, Id}).