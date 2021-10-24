%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%%% Awareness storage. Keeps tracks of clients that have requested a
%%% specific entity to notify them when the entity gets updated.

-module(sweet_awareness).
-behaviour(gen_server).
-author("Yamka").
-license("MPL-2.0").
-description("Awareness storage (backed by Cassandra)").

-include("../entities/entity.hrl").
-include_lib("cqerl/include/cqerl.hrl").

-export([init/1, handle_call/3, handle_cast/2]).
-export([start_link/0, stop/0,
         add/2, remove/2, remove/1, notify/2]).

-record(state, {}).

%%% gen_server callbacks

init(_) ->
    ets:new(awareness, [bag, named_table]),
    {ok, #state{}}.

handle_call({add, Key={_Type, _Id}, MainProcess}, _From, State) ->
    ets:insert(awareness, {Key, MainProcess}),
    lager:debug("~p is now aware of ~p", [MainProcess, Key]),
    {reply, ok, State};

handle_call({remove, Key={_Type, _Id}, MainProcess}, _From, State) ->
    ets:delete_object(awareness, {Key, MainProcess}),
    lager:debug("~p is no longer aware of ~p", [MainProcess, Key]),
    {reply, ok, State};

handle_call({remove, MainProcess}, _From, State) ->
    ets:match_delete(awareness, {'_', MainProcess}),
    lager:debug("~p is now not aware of anything", [MainProcess]),
    {reply, ok, State};

handle_call({notify, Key={_Type, _Id}, #entity{}=Entity}, _From, State) ->
    Nodes = [node()|nodes()],
    [gen_server:cast({awareness_server, Node}, {notify, Key, Entity}) || Node <- Nodes],
    lager:debug("broadcasted ~p to ~p nodes", [Entity, length(Nodes)]),
    {reply, ok, State}.

handle_cast({notify, Key={_Type, _Id}, #entity{}=Entity}, State) ->
    Objects = ets:lookup(awareness, Key),
    Packet = entities_packet:make([Entity]),
    [sweet_main:transmit(Pid, Packet) || {_, Pid} <- Objects],
    {noreply, State}.

%%% API

%% starts the server
-spec start_link() -> gen_server:start_link().
start_link() -> gen_server:start_link({local, awareness_server}, ?MODULE, [], []).

%% stops the server
-spec stop() -> gen_server:stop().
stop() -> gen_server:stop(awareness_server, ?MODULE, [], []).

%% Remembers that the specified MainProcess' client has requested
%% an entity
-spec add({atom(), integer()}, pid()) -> ok.
add({_Type, _Id}=Entity, MainProcess) -> gen_server:call(awareness_server, {add, Entity, MainProcess}).

%% Forgets that the specified MainProcess' client has requested
%% an entity
-spec remove({atom(), integer()}, pid()) -> ok.
remove({_Type, _Id}=Entity, MainProcess) -> gen_server:call(awareness_server, {remove, Entity, MainProcess}).

%% Forgets the specified MainProcess
-spec remove(pid()) -> ok.
remove(MainProcess) -> gen_server:call(awareness_server, {remove, MainProcess}).

%% Notifies all main processes about an entity update
-spec notify({atom(), integer()}, #entity{}) -> ok.
notify({_Type, _Id}=Spec, #entity{}=Entity) -> gen_server:call(awareness_server, {notify, Spec, Entity}).