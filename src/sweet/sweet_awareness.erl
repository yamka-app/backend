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
         add/2, remove/2, remove/1, notify/2, purge/0]).

-record(state, {}).

%%% gen_server callbacks

init(_) ->
    {ok, #state{}}.

handle_call({add, {Type, Id}, MainProcess}, _From, State) ->
    {ok, _} = cqerl:run_query(get(cassandra), #cql_query{
        statement = "INSERT INTO awareness (node, pid, type, id) values (?, ?, ?, ?)",
        values = [
            {node, node()},
            {pid, pid_to_list(MainProcess)},
            {type, Type},
            {id, Id}
        ]
    }),
    logging:dbg("~p is now aware of ~p", [MainProcess, {Type, Id}]),
    {reply, ok, State};

handle_call({remove, {Type, Id}, MainProcess}, _From, State) ->
    {ok, _} = cqerl:run_query(get(cassandra), #cql_query{
        statement = "DELETE FROM awareness WHERE node=? AND pid=? AND type=? AND id=?",
        values = [
            {node, node()},
            {pid, pid_to_list(MainProcess)},
            {type, Type},
            {id, Id}
        ]
    }),
    logging:dbg("~p is no longer aware of ~p", [MainProcess, {Type, Id}]),
    {reply, ok, State};

handle_call({remove, MainProcess}, _From, State) ->
    {ok, _} = cqerl:run_query(get(cassandra), #cql_query{
        statement = "DELETE FROM awareness WHERE node=? AND pid=?",
        values = [
            {node, node()},
            {pid, pid_to_list(MainProcess)}
        ]
    }),
    logging:dbg("~p is now not aware of anything", [MainProcess]),
    {reply, ok, State};

handle_call({notify, {TypeAtom, Id}, #entity{}=Entity}, _From, State) ->
    {ok, Result} = cqerl:run_query(get(cassandra), #cql_query{
        statement = "SELECT node FROM awareness_by_type_and_id WHERE type=? AND id=?",
        values = [
            {type, maps:get(TypeAtom, ?REVERSE_ENTITY_MAP)},
            {id, Id}
        ]
    }),
    Nodes = utils:unique([list_to_existing_atom(Node) || [{node, Node}] <- cqerl:all_rows(Result)]),
    [gen_server:cast({awareness_server, Node}, {notify, Entity}) || Node <- Nodes],
    logging:dbg("broadcasted ~p to ~p nodes", [Entity, length(Nodes)]),
    {reply, ok, State};

handle_call(purge, _From, State) ->
    {ok, _} = cqerl:run_query(get(cassandra), #cql_query{
        statement = "DELETE FROM awareness WHERE node=?",
        values = [{node, node()}]
    }),
    logging:dbg("awareness purged", []),
    {reply, ok, State}.

handle_cast({notify, {TypeAtom, Id}, #entity{}=Entity}, State) ->
    {ok, Result} = cqerl:run_query(get(cassandra), #cql_query{
        statement = "SELECT pid FROM awareness_by_type_and_id WHERE type=? AND id=? AND node=?",
        values = [
            {type, maps:get(TypeAtom, ?REVERSE_ENTITY_MAP)},
            {id, Id},
            {node, node()}
        ]
    }),
    Pids = [list_to_pid(Pid) || [{pid, Pid}] <- cqerl:all_rows(Result)],
    Packet = entities_packet:make([Entity]),
    [Pid ! {transmit, self(), Packet} || Pid <- Pids],
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

%% Forgets all processes bound to this node
-spec purge() -> ok.
purge() -> gen_server:call(awareness_server, purge).