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

-include_lib("cqerl/include/cqerl.hrl").

-export([init/1, handle_call/3, handle_cast/2]).
-export([start_link/0, stop/0,
         add/2, remove/2, remove/1, notify/1, purge/0]).

-record(state, {}).

%%% gen_server callbacks

init(_) ->
    {ok, #state{}}.

handle_call({add, Id, MainProcess}, _From, State) ->
    {ok, _} = cqerl:run_query(get(cassandra), #cql_query{
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
    {ok, _} = cqerl:run_query(get(cassandra), #cql_query{
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
    {ok, _} = cqerl:run_query(get(cassandra), #cql_query{
        statement = "DELETE FROM owners WHERE node=? AND pid=?",
        values = [
            {node, node()},
            {pid, pid_to_list(MainProcess)}
        ]
    }),
    logging:dbg("~p is no longer an owner of anything", [MainProcess]),
    {reply, ok, State};

handle_call({notify, Id}, _From, State) ->
    {ok, Result} = cqerl:run_query(get(cassandra), #cql_query{
        statement = "SELECT node FROM owners_by_id WHERE id=?",
        values = [
            {id, Id}
        ]
    }),
    Nodes = utils:unique([list_to_existing_atom(Node) || [{node, Node}] <- cqerl:all_rows(Result)]),
    [gen_server:cast({owner_server, Node}, {notify, Id}) || Node <- Nodes],
    logging:dbg("broadcasted updates to owners across ~p nodes", [length(Nodes)]),
    {reply, ok, State};

handle_call(purge, _From, State) ->
    {ok, _} = cqerl:run_query(get(cassandra), #cql_query{
        statement = "DELETE FROM owners WHERE node=?",
        values = [{node, node()}]
    }),
    logging:dbg("ownership purged", []),
    {reply, ok, State};

handle_call(_, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({notify, Id}, State) ->
    {ok, Result} = cqerl:run_query(get(cassandra), #cql_query{
        statement = "SELECT pid FROM owners_by_id WHERE id=? AND node=?",
        values = [
            {id, Id},
            {node, node()}
        ]
    }),
    Pids = [list_to_pid(Pid) || [{pid, Pid}] <- cqerl:all_rows(Result)],
    Packet = entities_packet:make([entity:get_record(user, Id)]),
    [Pid ! {transmit, self(), Packet} || Pid <- Pids],
    {noreply, State};

handle_cast(_, State) ->
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
-spec notify(integer()) -> ok.
notify(Id) -> gen_server:call(owner_server, {notify, Id}).

%% Forgets all processes bound to this node
-spec purge() -> ok.
purge() -> gen_server:call(owner_server, purge).