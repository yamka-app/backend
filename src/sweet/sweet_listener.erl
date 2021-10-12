%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(sweet_listener).
-author("Yamka").
-license("MPL-2.0").
-description("Accepts TLS clients").

-export([client_count/0, stop/0, start/2]).
-export([setup/3, acceptor_loop/1]).

start(Cassandra, Port) ->
    TlsConfig = {
        "/run/secrets/tls_fullchain",
        "/run/secrets/tls_fullchain",
        "/run/secrets/tls_privkey"
    },
    register(sweet_listener, spawn(?MODULE, setup, [Cassandra, Port, TlsConfig])).

stop() ->
    sweet_listener ! {stop, self()},
    unregister(sweet_listener).

setup(Cassandra, Port, {CertPath, CaCertPath, KeyPath}) ->
    % listen for new clients
    {ok, ListenSocket} = ssl:listen(Port, [
        {certfile,   CertPath},
        {cacertfile, CaCertPath},
        {keyfile,    KeyPath},
        {verify,     verify_none},
        {reuseaddr,  true},
        {versions,   ['tlsv1.2', 'tlsv1.3']},
        {active,     false}
    ]),
    spawn_link(?MODULE, acceptor_loop, [ListenSocket]),
    logging:log("Sweet listener running (port ~p)", [Port]),
    loop(Cassandra).

acceptor_loop(ListenSocket) ->
    {ok, Socket} = ssl:transport_accept(ListenSocket),
    sweet_listener ! {new_client, self(), Socket},
    acceptor_loop(ListenSocket).

loop(Cassandra) ->
    receive
        % start a main process when a client arrives
        {new_client, _From, Socket} ->
            logging:dbg("Client connected", []),
            spawn_monitor(sweet_main, start, [Socket, Cassandra]),
            loop(Cassandra);

        % clean the state up when a main process dies
        {'DOWN', _, process, Pid, _} ->
            logging:log("Main client process died", []),
            sweet_awareness:remove(Pid),
            sweet_owners:remove(Pid),
            loop(Cassandra);

        % shutdown when asked
        {stop, _From} ->
            logging:log("Sweet listener shutting down", []),
            ok
    end.

client_count() -> 0.