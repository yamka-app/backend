%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(client).
-author("Yamka").
-license("MPL-2.0").
-description("\"Normal protocol\" client process").

-define(TIMEOUT, 30*1000).
-define(MIN_PROTO, 12).
-define(MAX_PROTO, 14).
-include("entities/entity.hrl").
-include("packets/packet.hrl").
-include_lib("cqerl/include/cqerl.hrl").

-export([client_init/2, icpc_init/3]).
-export([icpc_broadcast_entity/2, icpc_broadcast_entity/3]).
-export([icpc_broadcast_to_aware/1, icpc_broadcast_to_aware/2, icpc_broadcast_to_aware/3, icpc_broadcast_to_aware/4]).

%%% handles client packets

%% identification packet (protocol version, compression support)
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


%% login packet (to acquire an access token)
handle_packet(#packet{type=login, seq=Seq,
                      fields=#{email   :=Email,
                               password:=SentPass,
                               perms   :=Permissions,
                               agent:=#entity{type=agent, fields=Agent}}}, ScopeRef) ->
    % ensure proper connection state
    {_, awaiting_login} = {{ScopeRef, status_packet:make_invalid_state(awaiting_login, Seq)}, get(state)},
    % rate limiting
    {_, 1} = {{ScopeRef, status_packet:make(rate_limiting, "Too many attempts. Please try again in a minute", Seq)}, ratelimit:hit(login)},
    % get the user and ensure they're the only one with this email (could be none)
    {ok, User} = cqerl:run_query(get(cassandra), #cql_query{
        statement = "SELECT salt, password, mfa_secret, id FROM users_by_email WHERE email=?",
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
    % create an agent or use an existing one
    AgentId = case Agent of
        #{id := Existing}             -> Existing;
        #{type := Type, name := Name} -> agent_e:create(Id, Type, Name)
    end,
    % generate the token depending on whether the client has 2FA enabled or not
    case MfaSecret of
        null -> access_token_packet:make(yamka_auth:create_token(Permissions, AgentId), Seq);
        Secret ->
            put(mfa_id, Id),
            put(mfa_agent, AgentId),
            put(mfa_perms, Permissions),
            put(mfa_secret, Secret),
            put(state, awaiting_mfa),
            status_packet:make(mfa_required, "2FA is enabled on this account", Seq)
    end;


%% MFA "secret" packet (to finish 2FA authentication)
handle_packet(#packet{type=mfa_secret, seq=Seq,
                      fields=#{secret:=Token}}, ScopeRef) ->
    % ensure proper connection state
    {_, awaiting_mfa} = {{ScopeRef, status_packet:make_invalid_state(awaiting_login, Seq)}, get(state)},
    % rate limiting
    {_, 1} = {{ScopeRef, status_packet:make(rate_limiting, "Please try again in a minute", Seq)}, ratelimit:hit(login)},
    % verify 2FA token
    {_, true} = {{ScopeRef, status_packet:make(login_error, "Invalid 2FA token", Seq)},
            yamka_auth:totp_verify(get(mfa_secret), Token)},
    put(mfa_secret, none),
    put(state, awaiting_login),
    access_token_packet:make(yamka_auth:create_token(get(mfa_perms), get(mfa_agent)), Seq);


%% signup packet (to create an account)
handle_packet(#packet{type=signup, seq=Seq,
                      fields=#{email   :=EMail,
                               password:=SentPass,
                               name    :=Name,
                               agent:=#entity{type=agent, fields=Agent}}}, ScopeRef) ->
    % ensure proper connection state
    {_, awaiting_login} = {{ScopeRef, status_packet:make_invalid_state(awaiting_login, Seq)}, get(state)},
    % check if the E-Mail is valid
    {_, true} = {{ScopeRef, status_packet:make(signup_error, "Invalid E-Mail", Seq)},
        email:is_valid(EMail)},
    {_, true} = {{ScopeRef, status_packet:make(signup_error, "Use a longer password", Seq)},
        length(SentPass) >= 6},
    {_, true} = {{ScopeRef, status_packet:make(signup_error, "The name is too long or too short", Seq)},
        (length(Name) >= 3) and (length(Name) =< 64)},
    {_, false} = {{ScopeRef, status_packet:make(signup_error, "E-Mail is already in use", Seq)},
        user_e:email_in_use(EMail)},
    Id = user_e:create(Name, EMail, SentPass),
    % create an agent or use an existing one
    AgentId = case Agent of
        #{id := Existing}              -> Existing;
        #{type := Type, name := AName} -> agent_e:create(Id, Type, AName)
    end,
    access_token_packet:make(yamka_auth:create_token(?ALL_PERMISSIONS_EXCEPT_BOT, AgentId), Seq);


%% access token packet (to identify the user and permissions)
handle_packet(#packet{type=access_token, seq=Seq,
                      fields=#{token := Token}}, ScopeRef) ->
    % ensure proper connection state
    {_, awaiting_login} = {{ScopeRef, status_packet:make_invalid_state(awaiting_login, Seq)}, get(state)},
    % get the token
    {_, {AgentId, Perms}} = {{ScopeRef, status_packet:make(invalid_access_token, "Invalid token")}, yamka_auth:get_token(Token)},
    #{owner := Id} = agent_e:get(AgentId),
    % create an ICPC process
    spawn_link(?MODULE, icpc_init, [self(), get(socket),
            {Id, AgentId, Perms, get(protocol), get(supports_comp)}]),
    % save state
    put(state, normal),
    put(id, Id),
    put(agent, AgentId),
    put(perms, Perms),
    ets:insert(id_of_processes, {self(), Id, AgentId}),
    user_e:broadcast_status(Id),
    client_identity_packet:make(Id, AgentId, Seq);


%% entity get packet (to request a set of entities)
handle_packet(#packet{type=entity_get, seq=Seq,
                      fields=#{entities := Entities}}, ScopeRef) ->
    {_, normal} = {{ScopeRef, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    entities_packet:make([entity:handle_get_request(R, {ScopeRef, Seq}) || R <- Entities], Seq);


%% entity packet (to put a set of entities)
handle_packet(#packet{type=entities, seq=Seq,
                      fields=#{entities := Entities}}, ScopeRef) ->
    {_, normal} = {{ScopeRef, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    {_, false} = {{ScopeRef, status_packet:make_excessive_data(Seq)}, entity:check_excessivity(Entities)},
    [entity:handle_entity(R, Seq, ScopeRef) || R <- Entities];


%% file download request (to download a file)
handle_packet(#packet{type=file_download_request, seq=Seq,
                      fields=#{id := Id}}, ScopeRef) ->
    {_, normal} = {{ScopeRef, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    {_, true} = {{ScopeRef, status_packet:make(invalid_id, "Unknown ID", Seq)}, file_storage:exists(Id)},
    file_storage:send_file(Id, Seq, {get(socket), get(protocol)}),
    none;


%% file data chunk (to upload a file)
handle_packet(#packet{type=file_data_chunk, seq=Seq,
                      fields=#{data := Data}}, ScopeRef) ->
    {_, normal} = {{ScopeRef, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    get(file_recv_pid) ! Data,
    none;


%% accept friend request
handle_packet(#packet{type=contacts_manage, seq=Seq,
                      fields=#{type:=friend, action:=add, id:=Id}}, ScopeRef) ->
    {_, normal} = {{ScopeRef, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    yamka_auth:assert_permission(edit_relationships, {ScopeRef, Seq}),
    % check if that user has, in fact, sent a friend request
    #{pending_in := Requests} = user_e:get(get(id)),
    {_, true} = {{ScopeRef, status_packet:make(contact_action_not_applicable, "This user has not issued a friend request", Seq)},
        lists:member(Id, Requests)},
    % write to DB
    DmId = channel_e:create("DM", 0, null, true),
    user_e:add_dm_channel([Id, get(id)], DmId),
    user_e:accept_friend_rq(Id, get(id)),
    % broadcast changes
    icpc_broadcast_entity(Id,      #entity{type=user, fields=user_e:get(Id)},      [id, friends, pending_out]),
    icpc_broadcast_entity(get(id), #entity{type=user, fields=user_e:get(get(id))}, [id, friends, pending_in]),
    none;
%% block
handle_packet(#packet{type=contacts_manage, seq=Seq,
                      fields=#{type:=blocked, action:=add, id:=Id}}, ScopeRef) ->
    {_, normal} = {{ScopeRef, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    yamka_auth:assert_permission(edit_relationships, {ScopeRef, Seq}),
    % write to DB
    user_e:remove_dm_channel([Id, get(id)]),
    user_e:remove_friend(Id, get(id)),
    user_e:block(Id, get(id)),
    % broadcast changes
    icpc_broadcast_entity(Id,      #entity{type=user, fields=user_e:get(Id)},      [id, friends]),
    icpc_broadcast_entity(get(id), #entity{type=user, fields=user_e:get(get(id))}, [id, friends, blocked]),
    none;
%% decline friend request
handle_packet(#packet{type=contacts_manage, seq=Seq,
                      fields=#{type:=pending_in, action:=remove, id:=Id}}, ScopeRef) ->
    {_, normal} = {{ScopeRef, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    yamka_auth:assert_permission(edit_relationships, {ScopeRef, Seq}),
    % write to DB
    user_e:decline_friend_rq(Id, get(id)),
    % broadcast changes
    icpc_broadcast_entity(Id,      #entity{type=user, fields=user_e:get(Id)},      [id, pending_out]),
    icpc_broadcast_entity(get(id), #entity{type=user, fields=user_e:get(get(id))}, [id, pending_in]),
    none;
%% cancel a friend request we sent
handle_packet(#packet{type=contacts_manage, seq=Seq,
                      fields=#{type:=pending_out, action:=remove, id:=Id}}, ScopeRef) ->
    {_, normal} = {{ScopeRef, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    yamka_auth:assert_permission(edit_relationships, {ScopeRef, Seq}),
    % write to DB
    user_e:decline_friend_rq(get(id), Id),
    % broadcast changes
    icpc_broadcast_entity(get(id), #entity{type=user, fields=user_e:get(get(id))}, [id, pending_out]),
    icpc_broadcast_entity(Id,      #entity{type=user, fields=user_e:get(Id)},      [id, pending_in]),
    none;
%% remove friend
handle_packet(#packet{type=contacts_manage, seq=Seq,
                      fields=#{type:=friend, action:=remove, id:=Id}}, ScopeRef) ->
    {_, normal} = {{ScopeRef, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    yamka_auth:assert_permission(edit_relationships, {ScopeRef, Seq}),
    % write to DB
    user_e:remove_friend(Id, get(id)),
    % broadcast changes
    icpc_broadcast_entity(get(id), #entity{type=user, fields=user_e:get(get(id))}, [id, friends]),
    icpc_broadcast_entity(Id,      #entity{type=user, fields=user_e:get(Id)},      [id, friends]),
    none;
%% unblock
handle_packet(#packet{type=contacts_manage, seq=Seq,
                      fields=#{type:=blocked, action:=remove, id:=Id}}, ScopeRef) ->
    {_, normal} = {{ScopeRef, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    yamka_auth:assert_permission(edit_relationships, {ScopeRef, Seq}),
    % write to DB
    user_e:unblock(Id, get(id)),
    % broadcast changes
    icpc_broadcast_entity(get(id), #entity{type=user, fields=user_e:get(get(id))}, [id, blocked]),
    none;
%% leave group
handle_packet(#packet{type=contacts_manage, seq=Seq,
                      fields=#{type:=group, action:=remove, id:=Id}}, ScopeRef) ->
    {_, normal} = {{ScopeRef, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    yamka_auth:assert_permission(edit_relationships, {ScopeRef, Seq}),
    % write to DB
    #{everyone_role := Everyone} = group_e:get(Id),
    role_e:remove(Everyone, get(id)),
    % broadcast changes
    icpc_broadcast_entity(get(id), #entity{type=user, fields=user_e:get(get(id))}, [id, groups]),
    none;
%% invalid request
handle_packet(#packet{type=contacts_manage, seq=Seq}, ScopeRef) ->
    {_, normal} = {{ScopeRef, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    yamka_auth:assert_permission(edit_relationships, {ScopeRef, Seq}),
    status_packet:make(contact_action_not_applicable, "Invalid request (check type and target id)", Seq);


%% user search packet (send a friend request using their name and tag)
handle_packet(#packet{type=search, seq=Seq,
                      fields=#{type:=user, name:=Name}}, ScopeRef) ->
    {_, normal} = {{ScopeRef, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    yamka_auth:assert_permission(edit_relationships, {ScopeRef, Seq}),
    {_, {ok, Id}} = {{ScopeRef, status_packet:make(invalid_username, "Invalid username", Seq)},
        utils:safe_call(fun user_e:search/1, [Name], [{cassandra, get(cassandra)}])},
    % write and broadcast changes
    user_e:send_friend_rq(get(id), Id),
    icpc_broadcast_entity(get(id), #entity{type=user, fields=user_e:get(get(id))}, [pending_out]),
    icpc_broadcast_entity(Id,      #entity{type=user, fields=user_e:get(Id)},      [pending_in]),
    status_packet:make(friend_request_sent, "Friend request sent", Seq);


%% group member search
handle_packet(#packet{type=search, seq=Seq,
                      fields=#{type:=group_member, name:=Name, ref:=Id}}, ScopeRef) ->
    {_, normal} = {{ScopeRef, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    yamka_auth:assert_permission(see_groups, {ScopeRef, Seq}),
    Users = group_e:find_users(Id, Name, 5),
    search_result_packet:make(Users, Seq);


%% group emoji search
handle_packet(#packet{type=search, seq=Seq,
                      fields=#{type:=group_emoji, name:=Name, ref:=Id}}, ScopeRef) ->
    {_, normal} = {{ScopeRef, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    yamka_auth:assert_permission(see_groups, {ScopeRef, Seq}),
    Emoji = group_e:find_emoji(Id, Name, 10),
    search_result_packet:make(Emoji, Seq);


%% invite resolution packet (to get the group by one of its invites)
handle_packet(#packet{type=invite_resolve, seq=Seq,
                      fields=#{code:=Code, add:=Add}}, ScopeRef) ->
    {_, normal} = {{ScopeRef, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    yamka_auth:assert_permission(join_groups, {ScopeRef, Seq}),
    {_, {ok, Id}} = {{ScopeRef, status_packet:make(invalid_invite, "Invalid invite", Seq)},
        group_e:resolve_invite(Code)},
    case Add of
        false -> ok;
        true ->
            #{name := SelfName} = user_e:get(get(id)),
            #{everyone_role := Everyone} = group_e:get(Id),
            role_e:add(Everyone, get(id)),
            group_e:cache_user_name(Id, get(id), SelfName),
            icpc_broadcast_entity(get(id),
                #entity{type=user, fields=user_e:get(get(id))}, [groups])
    end,
    entities_packet:make([#entity{type=group, fields=group_e:get(Id, false)}], Seq);


%% voice join packet
handle_packet(#packet{type=voice_join, seq=Seq,
                      fields=#{channel:=Chan, crypto:=Key}}, ScopeRef) ->
    {_, normal} = {{ScopeRef, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    {_, 1} = {{ScopeRef, status_packet:make(rate_limiting, "Rate limiting")}, ratelimit:hit(voice)},
    Session = tasty:create_session(Key, get(id), Chan),
    Server = tasty:server_name(),
    logging:log("Redirecting voice client to ~p", [Server]),
    voice_join_packet:make(Session, Server, Seq);


%% email confirmation packet
handle_packet(#packet{type=email_confirmation, seq=Seq,
                      fields=#{code:=Code}}, ScopeRef) ->
    {_, normal} = {{ScopeRef, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    {_, ok} = {{ScopeRef, status_packet:make(invalid_confirmation_code,
                          "Invalid email address confirmation code", Seq)},
        user_e:finish_email_confirmation(get(id), Code)},
    entities_packet:make([#entity{type=user, fields=#{id => get(id), email_confirmed => true}}]);


%% password change packet
handle_packet(#packet{type=password_change, seq=Seq,
                      fields=#{old_pass:=OldPass, mfa_code:=MfaCode, pass:=Pass}}, ScopeRef) ->
    {_, normal} = {{ScopeRef, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    {_, true} = {{ScopeRef, status_packet:make(invalid_credential, "Use a longer password", Seq)},
        length(Pass) >= 6},
    % make sure the old password is correct
    {ok, Rows} = cqerl:run_query(get(cassandra), #cql_query{
        statement = "SELECT salt, password, mfa_secret FROM users WHERE id=?",
        values    = [{id, get(id)}]
    }),
    User = cqerl:head(Rows),
    Salt       = proplists:get_value(salt, User),
    ExPassword = proplists:get_value(password, User),
    MfaSecret  = proplists:get_value(mfa_secret, User),
    {_, ExPassword} = {{ScopeRef, status_packet:make(invalid_credential, "Invalid current password", Seq)},
        utils:hash_password(OldPass, Salt)},
    % check the 2FA code
    MfaCheckPassed = case MfaSecret of
        null -> true;
        _ -> yamka_auth:totp_verify(MfaSecret, MfaCode)
    end,
    {_, true} = {{ScopeRef, status_packet:make(invalid_credential, "Invalid 2FA code", Seq)},
        MfaCheckPassed},
    % change the password if all checks passed
    NewHash = utils:hash_password(Pass, Salt),
    {ok, _} = cqerl:run_query(get(cassandra), #cql_query{
        statement = "UPDATE users SET password=? WHERE id=?",
        values    = [{id, get(id)}, {password, NewHash}]
    }),
    status_packet:make(password_changed, "Changed password successfully", Seq);


%% 2FA change packet
handle_packet(#packet{type=mfa_toggle, seq=Seq,
                      fields=#{pass:=Pass, enable:=Enable}}, ScopeRef) ->
    {_, normal} = {{ScopeRef, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    % make sure the password is correct
    {_, true} = {{ScopeRef, status_packet:make(invalid_credential, "Invalid password", Seq)},
        yamka_auth:pass_verify(get(id), Pass)},
    % broadcast the changes
    icpc_broadcast_entity(get(id), #entity{type=user,
        fields=#{id => get(id), mfa_enabled => Enable}}, [mfa_enabled]),
    % reply with nothing or with our newly generated secret
    if
        Enable ->
            Secret = yamka_auth:totp_secret(),
            user_e:update(get(id), #{mfa_secret => Secret}),
            mfa_secret_packet:make(Secret, Seq);
        true ->
            user_e:update(get(id), #{mfa_secret => null}),
            status_packet:make(mfa_toggled, "2FA disabled", Seq)
    end;

%% ping packet (to signal to the server that the connection is alive)
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
        {error, _} ->
            logging:log("connection to ~w closed", [get(client_ip)]),
            ssl:close(get(socket)),
            stop;

        {error, decoding, Seq, Type, DErr} ->
            if
                DErr =:= unknown_packet ->
                    status_packet:make(unknown_packet, "Unknown packet type");
                (Type =/= identification) and (State =:= awaiting_identification) ->
                    status_packet:make(invalid_connection_state, "Protocol version unknown or illegal");
                true ->
                    logging:warn("~w: decoding error of seq ~w (~p)", [get(client_ip), Seq, DErr]),
                    status_packet:make(packet_parsing_error, "Packet parsing failed. Please check structures with the docs", Seq)
            end;
        
        {ok, Packet} ->
            if
                (Packet#packet.type =/= identification) and (State =:= awaiting_identification) ->
                    status_packet:make(invalid_connection_state, "Protocol version unknown or illegal");
                true ->
                    % thanks to José M at https://stackoverflow.com/a/65711977/8074626
                    % for this graceful match error handling technique
                    try
                        % impose rate limits on all types of packets except for file chunks
                        if
                            Packet#packet.type =/= file_data_chunk ->
                                % terminate the connection if the client sends us too many packets
                                {_, 1} = {{ScopeRef, close}, ratelimit:hit(close)},
                                % ignore the packet if the client sends us too many of them, but not as many to close the connection
                                {_, 1} = {{ScopeRef, status_packet:make_rate_limiting(Packet)}, ratelimit:hit(packet)};
                            true -> ok
                        end,
                        logging:dbg("--> ~p", [packet_iface:clear_for_printing(Packet)]),
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
        stop          -> stop;
        none          -> continue;
        close         -> ssl:close(get(socket)), stop;
        Packets=[_|_] -> lists:foreach(fun send_packet/1, Packets), continue;
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
    ratelimit:make(packet,  {100, 1000  }),
    ratelimit:make(close,   {105, 1000  }),
    ratelimit:make(login,   {5,   30000 }),
    ratelimit:make(entity,  {300, 1000  }),
    ratelimit:make(message, {20,  10000 }),
    ratelimit:make(bot,     {1,   120000}),
    ratelimit:make(voice,   {2,   1000  }),

    % run the client loop
    try client_loop()
    catch
        Ex:Type:Trace ->
            % what do we do in this situation?
            % no idea
            % at least let's log it so we can take a look later
            ErrId = base64:encode(crypto:strong_rand_bytes(9)),
            logging:err("[~s] Internal error: ~p", [ErrId, {Ex, Type, Trace}]),
            send_packet(status_packet:make(internal_error, 
                lists:flatten(io_lib:format("An internal server error has occured [~s]", [ErrId])))),
            ssl:close(get(socket)),
            exit(crash)
    end.

%% sends a packet
send_packet(none) -> continue;
send_packet(P) ->
    ReplySeq = put(seq, get(seq) + 1) + 1,
    SeqPacket = P#packet{seq = ReplySeq},
    logging:dbg("<-- ~p", [packet_iface:clear_for_printing(SeqPacket)]),
    {WriterPid, _} = spawn_monitor(packet_iface, writer, [
        get(socket), SeqPacket,
        get(protocol), get(supports_comp), self()
    ]),
    receive
        {'DOWN', _, process, WriterPid, _} -> stop;
        {sent, ReplySeq}                   -> continue
    end.

%%% INTER-CLIENT PROCESS COMMUNICATION
%%% for things like sending entities around

icpc_broadcast_entity(Id, E) -> icpc_broadcast_entity(Id, E, []).
icpc_broadcast_entity(Id, E, F) -> icpc_broadcast(Id, {entities, [entity:filter(E, F)]}).
icpc_broadcast(Id, D) ->
    utils:broadcast(D, [P || {_,_,{_,P}} <- ets:lookup(icpc_processes, Id)]).

icpc_broadcast_to_aware(E)       -> icpc_broadcast_to_aware(E, maps:keys(E#entity.fields)).
icpc_broadcast_to_aware(E, F)    -> icpc_broadcast_to_aware(user_awareness, E, F).
icpc_broadcast_to_aware(T, E, F) -> icpc_broadcast_to_aware(T, maps:get(id, E#entity.fields), E, F).
icpc_broadcast_to_aware(T, Id, E, F) ->
    [icpc_broadcast_entity(RId, E, F)
        || {_,{RId,_}} <- ets:lookup(T, Id)].

icpc_init(Host, Socket, {Id, Agent, Perms, Protocol, SC}) ->
    put(socket, Socket), put(id, Id), put(agent, Agent), put(perms, Perms),
    put(protocol, Protocol), put(supports_comp, SC), put(seq, 0),
    % announce ourselves
    ets:insert(icpc_processes, {Id, Agent, {Host, self()}}),
    % start the loop
    icpc_loop().

icpc_loop() ->
    receive
        {entities, E} -> send_packet(entities_packet:make(E))
    end,
    icpc_loop().