%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(sweet_listener).
-behaviour(gen_server).
-author("Yamka").
-license("MPL-2.0").
-description("Accepts TLS clients").

-export([start_link/1, client_count/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, acceptor_loop/1]).

-record(state, {cassandra, listen_socket}).

acceptor_loop(ListenSocket) ->
    {ok, Socket} = ssl:transport_accept(ListenSocket),
    sweet_listener ! {new_client, Socket},
    acceptor_loop(ListenSocket).

start_link(Cassandra) -> gen_server:start_link({local, sweet_listener}, ?MODULE, Cassandra, []).

init(Cassandra) ->
    {ok, ListenSocket} = ssl:listen(yamka_config:get(sweet_port), [
        {certfile,   "/run/secrets/tls_fullchain"},
        {cacertfile, "/run/secrets/tls_fullchain"},
        {keyfile,    "/run/secrets/tls_privkey"},
        {verify,     verify_none},
        {reuseaddr,  true},
        {versions,   ['tlsv1.2', 'tlsv1.3']},
        {active,     false}
    ]),
    spawn_link(?MODULE, acceptor_loop, [ListenSocket]),
    {ok, #state{cassandra=Cassandra, listen_socket=ListenSocket}}.

handle_call(_, _, State) -> {reply, unknown_request, State}.
handle_cast(_, State) -> {noreply, State}.

handle_info({new_client, Socket}, State) ->
    lager:debug("client connected", []),
    % perform the handshake in a separate process
    Handshake = fun() ->
        {ok, TLS} = ssl:handshake(Socket),
        lager:debug("client handshake complete", []),
        sweet_dyn_sup:add_client(TLS)
    end,
    spawn(Handshake),
    {noreply, State};

handle_info({'DOWN', _, process, Pid, _}, State) ->
    lager:debug("main client process died", []),
    sweet_awareness:remove(Pid),
    sweet_owners:remove(Pid),
    {noreply, State}.

client_count() -> 0.