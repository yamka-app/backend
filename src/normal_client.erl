-module(normal_client).
-author("Order").
-license("MPL-2.0").
-description("\"Normal protocol\" client process").

-define(TIMEOUT, 30*1000).
-define(EMAIL_REGEX, "(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?|[a-z0-9-]*[a-z0-9]:(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)\\])").
-define(MIN_PROTO, 5).
-define(MAX_PROTO, 5).
-include("entities/entity.hrl").
-include("packets/packet.hrl").
-include_lib("cqerl/include/cqerl.hrl").

-export([client_init/2, icpc_init/2]).
-export([icpc_broadcast_entity/2, icpc_broadcast_entity/3]).
-export([safe_call/2, safe_call/3]).

%% handles client packets
handle_packet(#packet{type=identification, seq=Seq,
                      fields=#{supports_comp:=SupportsComp,
                               protocol     := Protocol}}, ScopeRef) ->
    {_, awaiting_identification} = {{ScopeRef, status_packet:make_invalid_state(awaiting_identification, Seq)}, get(state)},
    if
        (Protocol > ?MAX_PROTO) or (Protocol < ?MIN_PROTO) ->
            status_packet:make(unsupported_proto, "Unsupported protocol version", Seq);
        true -> 
            put(protocol, Protocol),
            put(supports_comp, SupportsComp),
            put(state, awaiting_login),
            none
    end;

handle_packet(#packet{type=login, seq=Seq,
                      fields=#{email   :=Email,
                               password:=SentPass,
                               perms   :=Permissions}}, ScopeRef) ->
    % ensure proper connection state
    {_, awaiting_login} = {{ScopeRef, status_packet:make_invalid_state(awaiting_login, Seq)}, get(state)},
    % rate limiting
    {_, 1} = {{ScopeRef, status_packet:make(rate_limiting, "Please try again in a minute", Seq)}, ratelimit:hit(login)},
    % get the user and ensure they're the only one with this email (could be none)
    {ok, User} = cqerl:run_query(get(cassandra), #cql_query{
        statement = "SELECT salt, password, mfa_secret, id FROM users WHERE email=?",
        values    = [{email, Email}]
   }),
    {_, 1} = {{ScopeRef, status_packet:make(login_error, "Invalid E-Mail", Seq)}, cqerl:size(User)},
    Row = cqerl:head(User),
    % check the password the user sent us
    Salt      = proplists:get_value(salt, Row),
    Password  = proplists:get_value(password, Row),
    MfaSecret = proplists:get_value(mfa_secret, Row),
    Id        = proplists:get_value(id, Row),
    {_, Password} = {{ScopeRef, status_packet:make(login_error, "Invalid password", Seq)}, utils:hash_password(SentPass, Salt)},
    % generate the token depending on whether the client has 2FA enabled or not
    case MfaSecret of
        null -> access_token_packet:make(auth:create_token(Permissions, Id), Seq);
        Secret ->
            put(mfa_secret, Secret),
            status_packet:make(mfa_required, "2FA is enabled on this account", Seq)
    end;

handle_packet(#packet{type=signup, seq=Seq,
                      fields=#{email   :=EMail,
                               password:=SentPass,
                               name    :=Name}}, ScopeRef) ->
    % ensure proper connection state
    {_, awaiting_login} = {{ScopeRef, status_packet:make_invalid_state(awaiting_login, Seq)}, get(state)},
    % check if the E-Mail is valid
    EMailLen = length(EMail),
    {ok, EMailRegex} = re:compile(?EMAIL_REGEX, [caseless]),
    {_, {match, [{0, EMailLen}]}} = {{ScopeRef, status_packet:make(signup_error, "Invalid E-Mail", Seq)},
        re:run(EMail, EMailRegex)},
    {_, true} = {{ScopeRef, status_packet:make(signup_error, "Use a longer password", Seq)},
        length(SentPass) >= 6},
    {_, true} = {{ScopeRef, status_packet:make(signup_error, "The name is too long or too short", Seq)},
        (length(Name) >= 3) and (length(Name) =< 64)},
    {_, false} = {{ScopeRef, status_packet:make(signup_error, "E-Mail is already in use", Seq)},
        user:email_in_use(EMail)},
    Id = user:create(Name, EMail, SentPass),
    access_token_packet:make(auth:create_token(?ALL_PERMISSIONS_EXCEPT_BOT, Id), Seq);

handle_packet(#packet{type=access_token, seq=Seq,
                      fields=#{token := Token}}, ScopeRef) ->
    % ensure proper connection state
    {_, awaiting_login} = {{ScopeRef, status_packet:make_invalid_state(awaiting_login, Seq)}, get(state)},
    % get the token
    {_, {Id, Perms}} = {{ScopeRef, status_packet:make(invalid_access_token, "Invalid token")}, auth:get_token(Token)},
    % create an ICPC process
    spawn(?MODULE, icpc_init, [get(socket), {Id, Perms, get(protocol), get(supports_comp)}]),
    % save state
    put(state, normal),
    put(id, Id),
    put(perms, Perms),
    client_identity_packet:make(Id, Seq);

handle_packet(#packet{type=entity_get, seq=Seq,
                      fields=#{entities := Entities}}, ScopeRef) ->
    {_, normal} = {{ScopeRef, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    entities_packet:make([entity:handle_get_request(R) || R <- Entities], Seq);

handle_packet(#packet{type=entities, seq=Seq,
                      fields=#{entities := Entities}}, ScopeRef) ->
    {_, normal} = {{ScopeRef, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    [entity:handle_entity(R, Seq, ScopeRef) || R <- Entities];

handle_packet(#packet{type=file_download_request, seq=Seq,
                      fields=#{id := Id}}, ScopeRef) ->
    {_, normal} = {{ScopeRef, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    {_, true} = {{ScopeRef, status_packet:make(invalid_id, "Unknown ID", Seq)}, file_storage:exists(Id)},
    file_storage:send_file(Id, Seq, {get(socket), get(protocol)}),
    none;

handle_packet(#packet{type=file_data_chunk, seq=Seq,
                      fields=#{data := Data}}, ScopeRef) ->
    {_, normal} = {{ScopeRef, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    get(file_recv_pid) ! Data,
    none;

handle_packet(#packet{type=contacts_manage, seq=Seq,
                      fields=#{type:=Type, action:=add, id:=Id}}, ScopeRef) ->
    {_, normal} = {{ScopeRef, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    Self = user:get(get(id)),
    {_, true} = {{ScopeRef, status_packet:make(contact_action_not_applicable, "Contact action not applicable")},
        (Type == blocked) or ((Type == friend) and lists:member(Id, maps:get(pending_in, Self))) },
    % write changes to the DB
    user:manage_contact(get(id), add, {Type, Id}),
    % if we're adding a friend, remove them from corresponding pending in/out queues
    if  Type == friend ->
            user:manage_contact(get(id), remove, {pending_in,  Id}),
            user:manage_contact(Id,      remove, {pending_out, get(id)});
        true -> ok
    end,
    % broadcast the changes to each of both users' devices
    icpc_broadcast_entity(get(id), #entity{type=user, fields=user:get(get(id))},
        [user:contact_field(Type), pending_in, pending_out]),
    Opposite = user:opposite_type(Type),
    if
        Opposite /= none ->
            icpc_broadcast_entity(Id, #entity{type=user, fields=user:get(Id)},
                [user:contact_field(Opposite), pending_in, pending_out]);
        true -> ok
    end,
    none;

handle_packet(#packet{type=contacts_manage, seq=Seq,
                      fields=#{type:=Type, action:=remove, id:=Id}}, ScopeRef) ->
    {_, normal} = {{ScopeRef, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    % write changes to the DB
    user:manage_contact(get(id), remove, {Type, Id}),
    % broadcast the changes to each of both users' devices
    icpc_broadcast_entity(get(id), #entity{type=user, fields=user:get(get(id))}, [user:contact_field(Type)]),
    Opposite = user:opposite_type(Type),
    icpc_broadcast_entity(Id, #entity{type=user, fields=user:get(Id)}, [user:contact_field(Opposite)]),
    none;

handle_packet(#packet{type=user_search, seq=Seq,
                      fields=#{name:=Name}}, ScopeRef) ->
    {_, normal} = {{ScopeRef, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    {_, {ok, Id}} = {{ScopeRef, status_packet:make(invalid_username, "Invalid username", Seq)},
        safe_call(fun user:search/1, [Name], [{cassandra, get(cassandra)}])},
    % write and broadcast changes
    user:manage_contact(get(id), add, {pending_out, Id}),
    icpc_broadcast_entity(get(id), #entity{type=user, fields=user:get(get(id))}, [pending_out]),
    icpc_broadcast_entity(Id,      #entity{type=user, fields=user:get(Id)},      [pending_in]),
    status_packet:make(friend_request_sent, "Friend request sent", Seq);

handle_packet(#packet{type=ping, seq=Seq,
                      fields=#{echo := Echo}}, _ScopeRef) ->
    #packet{type = pong, reply = Seq, fields = #{echo => Echo}};

handle_packet(_, _) -> status_packet:make(unknown_packet, "Unknown packet type").

%% the client loop
%% reads the client's packets and responds to them
client_loop() ->
    ScopeRef = make_ref(),
    % read a packet
    {ReaderPid, _} = spawn_monitor(packet_iface, reader, [get(socket), get(protocol), self()]),
    DecodingStatus =
        receive
            {'DOWN', _, process, ReaderPid, Reason} when Reason /= normal
                -> {error, Reason};
            {packet, P}                 -> {ok, P};
            {decoding_error, S, T, Err} -> {error, decoding, S, T, Err};
            upload_fin                  -> upload_fin

            after ?TIMEOUT              -> exit(ReaderPid, normal), {error, timeout}
        end,

    % weed out errors
    State = get(state),
    ReplyWith = case DecodingStatus of
        {error, E} ->
            logging:log("connection to ~w closed (~p)", [get(client_ip), E]),
            ssl:close(get(socket)),
            stop;

        {error, decoding, Seq, Type, DErr} ->
            if
                (Type /= identification) and (State == awaiting_identification) ->
                    status_packet:make(invalid_connection_state, "Protocol version unknown or illegal");
                true ->
                    logging:warn("~w: decoding error of seq ~w (~p)", [get(client_ip), Seq, DErr]),
                    status_packet:make(packet_parsing_error, "Packet parsing failed. Please check structures with the docs", Seq)
            end;
        
        {ok, Packet} ->
            if
                (Packet#packet.type /= identification) and (State == awaiting_identification) ->
                    status_packet:make(invalid_connection_state, "Protocol version unknown or illegal");
                true ->
                    % thanks to José M at https://stackoverflow.com/a/65711977/8074626
                    % for this graceful match error handling technique
                    try
                        % terminate the connection if the client sends us too many packets
                        {_, 1} = {{ScopeRef, close}, ratelimit:hit(close)},
                        % ignore the packet if the client sends us too many of them, but not as many to close the connection
                        {_, 1} = {{ScopeRef, status_packet:make_rate_limiting(Packet)}, ratelimit:hit(packet)},
                        logging:log("--> ~p", [packet_iface:clear_for_printing(Packet)]),
                        handle_packet(Packet, ScopeRef)
                    of
                        V -> V
                    catch
                        error:{badmatch, {{ScopeRef, Response}, _}} -> Response
                    end
            end;

        upload_fin -> put(file_recv_pid, none), none
    end,

    % send the response packet
    DoNext = case ReplyWith of
        stop   -> stop;
        none   -> continue;
        close  -> ssl:close(get(socket)), stop;
        [H|RT] -> lists:foreach(fun send_packet/1, [H|RT]), continue;
        ReplyPacket when is_record(ReplyPacket, packet) -> send_packet(ReplyPacket), continue
    end,

    % what to do next?
    case DoNext of
        stop     -> exit(ReaderPid, normal), ok;
        continue -> client_loop()
    end.

%% client init function
client_init(TransportSocket, Cassandra) ->
    % finish the handshake
    {ok, Socket} = ssl:handshake(TransportSocket),
    {ok, {ClientIP, _}} = ssl:peername(Socket),
    logging:log("~w connected to the normal server", [ClientIP]),

    % set initial state
    put(client_ip, ClientIP),
    put(socket, Socket),
    put(cassandra, Cassandra),
    put(protocol, 0), put(supports_comp, false),
    put(seq, 1),
    put(state, awaiting_identification),
    put(file_recv_pid, none),

    % make rate limiters
    ratelimit:make(packet,  {50,  1000  }),
    ratelimit:make(close,   {55,  1000  }),
    ratelimit:make(login,   {5,   30000 }),
    ratelimit:make(entity,  {150, 1000  }),
    ratelimit:make(message, {20,  10    }),
    ratelimit:make(bot,     {1,   120000}),

    % run the client loop
    try client_loop()
    catch
        Ex:Type:Trace ->
            % what do we do in this situation?
            % no idea
            % at least let's log it so we can take a look later
            logging:err("Internal error: ~p", [{Ex, Type, Trace}]),
            send_packet(status_packet:make(internal_error, "Sorry, an internal server error has occured")),
            ssl:close(get(socket)),
            exit(crash)
    end.

%% sends a packet
send_packet(none) -> continue;
send_packet(P) ->
    ReplySeq = put(seq, get(seq) + 1) + 1,
    SeqPacket = P#packet{seq = ReplySeq},
    logging:log("<-- ~p", [packet_iface:clear_for_printing(SeqPacket)]),
    {WriterPid, _} = spawn_monitor(packet_iface, writer, [
        get(socket), SeqPacket,
        get(protocol), get(supports_comp), self()
    ]),
    receive
        {'DOWN', _, process, WriterPid, _} -> stop;
        {sent, ReplySeq}                   -> continue
    end.

%% calls a function safely, trapping all exceptions
put_pd([]) -> ok;
put_pd([{K,V}|T]) -> put(K, V), put_pd(T).

safe_call(Fun, Args) -> safe_call(Fun, Args, []).
safe_call(Fun, Args, PD) ->
    Self = self(),
    Wrapper = fun() ->
            put_pd(PD),
            Self ! {ok, self(), apply(Fun, Args)}
        end,
    {Pid, _} = spawn_monitor(Wrapper),
    receive
        {ok, Pid, Val} -> {ok, Val};
        {'DOWN', _, process, Pid, Reason} -> {error, Reason}
    end.

%%% INTER-CLIENT PROCESS COMMUNICATION
%%% for things like sending entities around

icpc_broadcast_entity(Id, E) -> icpc_broadcast_entity(Id, E, []).
icpc_broadcast_entity(Id, E, F) -> icpc_broadcast(Id, {entities, [entity:filter(E, F)]}).
icpc_broadcast(Id, D) ->
    utils:broadcast(D, [P || {_,P} <- ets:lookup(icpc_processes, Id)]).

icpc_init(Socket, {Id, Perms, Protocol, SC}) ->
    put(socket, Socket), put(id, Id), put(perms, Perms),
    put(protocol, Protocol), put(supports_comp, SC), put(seq, 0),
    % announce ourselves
    ets:insert(icpc_processes, {Id, self()}),
    % start the loop
    icpc_loop().

icpc_loop() ->
    receive
        {entities, E} -> send_packet(entities_packet:make(E))
    end,
    icpc_loop().