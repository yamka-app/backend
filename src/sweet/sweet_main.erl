%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%%% TODO: make this a gen_server

-module(sweet_main).
-author("Yamka").
-license("MPL-2.0").
-description("Main server process for each client").

-include("../entities/entity.hrl").
-include("../packets/packet.hrl").

-record(state, {
    encoder :: pid(),
    decoder :: pid(),
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

-export([start/2]).
-export([switch_state/2, switch_state/3, send_packet/2, stop/1,
         ratelimit/2, ratelimit/3, get_state/1,
         route_to_aware/2, route_to_aware/3,
         route_to_owners/2, route_to_owners/3,
         get_file_recv_pid/1, set_file_recv_pid/2,
         get_user_info/1]).

spawn_helper(encoder, Socket, Setup={_,_}) -> spawn_monitor(sweet_encoder, start, [self(), Socket, Setup]);
spawn_helper(decoder, Socket, Proto) -> spawn_monitor(sweet_decoder, start, [self(), Socket, Proto]).

%% starts a main process
start(TransportSocket, Cassandra) ->
    % set up TLS
    {ok, Socket} = ssl:handshake(TransportSocket),

    % create rate limiters
    ratelimit:make(encoder_respawn, {1,   1000  }),
    ratelimit:make(decoder_respawn, {3,   1000  }),
    ratelimit:make(packet,          {100, 1000  }),
    ratelimit:make(close,           {105, 1000  }),
    ratelimit:make(login,           {5,   30000 }),
    ratelimit:make(entity,          {300, 1000  }),
    ratelimit:make(message,         {20,  10000 }),
    ratelimit:make(bot_create,      {1,   120000}),
    ratelimit:make(voice,           {2,   1000  }),

    % start some processes
    Encoder = spawn_helper(encoder, Socket, {0, false}),
    Decoder = spawn_helper(decoder, Socket, 0),
    loop(#state{
        encoder = Encoder,
        decoder = Decoder,
        socket = Socket,
        cassandra = Cassandra,
        conn_state = awaiting_identification,
        user_info = {},
        mfa_state = {},
        proto_ver = 0,
        comp = false,
        seq = 0,
        file_recv_pid = nopid
    }).

%% main loop
loop(State) ->
    #state{
        encoder = {EncPid, EncRef},
        decoder = {DecPid, DecRef},
        socket = Socket,
        cassandra = Cassandra,
        conn_state = ConnState,
        user_info = UserInfo,
        mfa_state = MfaState,
        proto_ver = ProtoVer,
        comp = SupportsCompression,
        seq = Seq,
        file_recv_pid = FileRecvPid
    } = State,

    receive
        % restart the decoder if it fails
        {'DOWN', DecRef, process, DecPid, Reason} ->
            lager:error("Decoder down (~p)", [Reason]),
            case ratelimit:hit(decoder_respawn) of
                1 ->
                    self() ! {transmit, self(), status_packet:make(packet_parsing_error, "Packet parsing error")},
                    loop(State#state{decoder = spawn_helper(decoder, Socket, ProtoVer)});
                0 ->
                    lager:error("Decoder respawn rate limit reached", []),
                    exit(respawn_limit_reached)
            end;

        % restart the encoder if it fails
        {'DOWN', EncRef, process, EncPid, Reason} ->
            lager:error("Encoder down (~p)", [Reason]),
            case ratelimit:hit(decoder_respawn) of
                1 ->
                    loop(State#state{encoder = spawn_helper(encoder, Socket, {ProtoVer, SupportsCompression})});
                0 ->
                    lager:error("Encoder respawn rate limit reached", []),
                    exit(respawn_limit_reached)
            end;

        % handle packets decoded by the decoder
        {packet, DecPid, Packet} ->
            lager:debug("--> ~p", [Packet]),
            Id = case UserInfo of
                {Val, _, _} -> Val;
                _ -> undefined
            end,
            spawn(sweet_handler, start, [self(), Id, Cassandra, Packet]),
            loop(State);

        % switch the state (and possibly the protocol version) of the protocol processes when the handler asks for it
        {switch_state, _From, awaiting_login, {NewVersion, NewCompression}} ->
            loop(State#state{
                decoder = spawn_helper(decoder, Socket, NewVersion),
                encoder = spawn_helper(encoder, Socket, {NewVersion, NewCompression}),
                conn_state = awaiting_login,
                proto_ver = NewVersion,
                comp = NewCompression
            });
        {switch_state, _From, awaiting_mfa, NewMfa} ->
            loop(State#state{conn_state = awaiting_mfa, mfa_state = NewMfa});
        {switch_state, _From, normal, {Id,_,_}=NewUserInfo} ->
            sweet_owners:add(Id, self()),
            loop(State#state{conn_state = normal, user_info = NewUserInfo});
        {switch_state, _From, NewState} ->
            loop(State#state{conn_state = NewState});

        % send packets when asked by a packet handler or another main process
        {transmit, _From, Packet} ->
            Seqd = Packet#packet{seq=Seq + 1}, % write seq
            lager:debug("<-- ~p", [Seqd]),
            EncPid ! {packet, self(), Seqd},
            loop(State#state{seq=Seq + 1});

        % route an entity to another main process when asked by a handler process of ours
        {route, _From, {aware, {_Type, _Id}=Spec}, Entity} ->
            sweet_awareness:notify(Spec, Entity),
            loop(State);
        {route, _From, {owners, Id}, Entity} ->
            sweet_owners:notify(Id, Entity),
            loop(State);

        % updates awareness
        {awareness, _From, Action, {_Type, _Id}=Entity} ->
            sweet_awareness:Action(Entity, self()),
            loop(State);

        % hits a rate limiter
        {ratelimit, From, Name, Times} ->
            From ! {result, self(), ratelimit:hit(Name, Times)},
            loop(State);

        % gets the connection state
        {get_state, From} ->
            From ! {result, self(), ConnState, MfaState},
            loop(State);

        % gets the file receiver PID
        {get_recv, From} ->
            From ! {result, self(), FileRecvPid},
            loop(State);

        % sets the file receiver PID
        {set_recv, _From, Pid} ->
            loop(State#state{file_recv_pid=Pid});

        % gets user info
        {get_user_info, From} ->
            From ! {result, self(), UserInfo},
            loop(State);

        % shutdown when asked
        {stop, _From} ->
            ssl:close(Socket),
            ok;

        Unknown ->
            lager:warning("Unknown sweet_main request: ~p", [Unknown]),
            loop(State)
    end.

%%%
%%% API functions so that other modules don't have to remember the message format spec
%%% 

%% switches the connection state
switch_state(Pid, awaiting_login, Setup={_,_}) -> Pid ! {switch_state, self(), awaiting_login, Setup};
switch_state(Pid, awaiting_mfa, Mfa) -> Pid ! {switch_state, self(), awaiting_mfa, Mfa};
switch_state(Pid, normal, UserInfo) -> Pid ! {switch_state, self(), normal, UserInfo}.
switch_state(Pid, Target) -> Pid ! {switch_state, self(), Target}.

%% sends a packet to the connected user
send_packet(Pid, P) -> Pid ! {transmit, self(), P}.

%% routes an entity using the destination spec
route_entity(Pid, DestSpec, E=#entity{fields=F}, Allowed) ->
    route_entity(Pid, DestSpec, E#entity{fields=maps:filter(fun(K, _) ->
        lists:member(K, Allowed) end, F)}).
route_entity(Pid, DestSpec, E) ->
    Pid ! {route, self(), DestSpec, E}.

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
    Pid ! {ratelimit, self(), Name, Times},
    receive
        {result, Pid, Result} -> Result
    after 1000 ->
        {error, timeout}
    end.

%% gets the connection state
get_state(Pid) ->
    Pid ! {get_state, self()},
    receive
        {result, Pid, Conn, Mfa} -> {ok, Conn, Mfa}
    after 1000 ->
        {error, timeout}
    end.

%% gets the file receiver PID
get_file_recv_pid(Pid) ->
    Pid ! {get_recv, self()},
    receive
        {result, Pid, RPid} -> {ok, RPid}
    after 1000 ->
        {error, timeout}
    end.

%% gets the file receiver PID
get_user_info(Pid) ->
    Pid ! {get_user_info, self()},
    receive
        {result, Pid, UI} -> UI
    after 1000 ->
        {error, timeout}
    end.

%% sets the file receiver PID
set_file_recv_pid(Pid, Recv) ->
    Pid ! {set_recv, self(), Recv}.

%% disconnects the client
stop(Pid) -> Pid ! {stop, self()}.