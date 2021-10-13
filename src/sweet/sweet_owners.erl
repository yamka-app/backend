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
-export([start_link/1, stop/0,
         add/2, remove/2, remove/1, notify/2, purge/0,
         is_online/1]).

-record(state, {cassandra}).

%%% gen_server callbacks

init(Cassandra) ->
    {ok, #state{cassandra=Cassandra}}.

handle_call({add, Id, MainProcess}, _From, State) ->
    {ok, _} = cqerl:run_query(State#state.cassandra, #cql_query{
        statement = "INSERT INTO owners (node, pid, id) values (?, ?, ?)",
        values = [
            {node, node()},
            {pid, pid_to_list(MainProcess)},
            {id, Id}
        ]
    }),
    logging:dbg("~p is now an owner of ~p", [MainProcess, Id]),
    {reply, ok, State};

handle_call({remove, Id, MainProcess}, _From, State) ->
    {ok, _} = cqerl:run_query(State#state.cassandra, #cql_query{
        statement = "DELETE FROM owners WHERE node=? AND pid=? AND id=?",
        values = [
            {node, node()},
            {pid, pid_to_list(MainProcess)},
            {id, Id}
        ]
    }),
    logging:dbg("~p is no longer an owner of ~p", [MainProcess, Id]),
    {reply, ok, State};

handle_call({remove, MainProcess}, _From, State) ->
    {ok, _} = cqerl:run_query(State#state.cassandra, #cql_query{
        statement = "DELETE FROM owners WHERE node=? AND pid=?",
        values = [
            {node, node()},
            {pid, pid_to_list(MainProcess)}
        ]
    }),
    logging:dbg("~p is no longer an owner of anything", [MainProcess]),
    {reply, ok, State};

handle_call({notify, Id, Entity}, _From, State) ->
    {ok, Result} = cqerl:run_query(State#state.cassandra, #cql_query{
        statement = "SELECT node FROM owners_by_id WHERE id=?",
        values = [{id, Id}]
    }),
    Nodes = utils:unique([list_to_existing_atom(Node) || [{node, Node}] <- cqerl:all_rows(Result)]),
    [gen_server:cast({owner_server, Node}, {notify, Id, Entity}) || Node <- Nodes],
    logging:dbg("broadcasted updates to owners across ~p nodes", [length(Nodes)]),
    {reply, ok, State};

handle_call(purge, _From, State) ->
    {ok, _} = cqerl:run_query(State#state.cassandra, #cql_query{
        statement = "DELETE FROM owners WHERE node=?",
        values = [{node, node()}]
    }),
    logging:dbg("ownership purged", []),
    {reply, ok, State};

handle_call({is_online, Id}, _From, State) ->
    {ok, Result} = cqerl:run_query(State#state.cassandra, #cql_query{
        statement = "SELECT count(pid) FROM owners_by_id WHERE id=?",
        values = [{id, Id}]
    }),
    [{'system.count(pid)', Cnt}] = cqerl:head(Result),
    {reply, Cnt >= 1, State}.

handle_cast({notify, Id, Entity}, State) ->
    {ok, Result} = cqerl:run_query(State#state.cassandra, #cql_query{
        statement = "SELECT pid FROM owners_by_id WHERE id=? AND node=?",
        values = [
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
-spec start_link(any()) -> gen_server:start_link().
start_link(Cassandra) -> gen_server:start_link({local, owner_server}, ?MODULE, Cassandra, []).

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

%% Forgets all processes bound to this node
-spec purge() -> ok.
purge() -> gen_server:call(owner_server, purge).

%% Determines if a user is online
-spec is_online(integer()) -> boolean().
is_online(Id) -> gen_server:call(owner_server, {is_online, Id}).