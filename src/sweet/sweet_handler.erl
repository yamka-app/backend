%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(sweet_handler).
-author("Yamka").
-license("MPL-2.0").
-description("Packet handler process for each packet").

-define(MIN_PROTO, 12). % lowest and highest allowed versions
-define(MAX_PROTO, 14).

-include("../packets/packet.hrl").
-include("../entities/entity.hrl").
-include_lib("cqerl/include/cqerl.hrl").

-export([start/5]).

%% asserts a connection state
assert_state(Main, Wanted, Scope) ->
    {_, {ok, Wanted}} = {{Scope, status_packet:make_invalid_state(Wanted)}, sweet_main:get_state(Main)}.

%% registers protocol information
handle_packet(#packet{type=identification,
                      fields=#{supports_comp := SupportsComp,
                               protocol := Protocol}}, Main, Scope) ->
    assert_state(Main, awaiting_identification, Scope),
    if
        (Protocol > ?MAX_PROTO) or (Protocol < ?MIN_PROTO) ->
            status_packet:make(unsupported_proto, "Unsupported protocol version");
        true ->
            sweet_main:switch_state(Main, awaiting_login, {Protocol, SupportsComp}),
            none
    end;


%% acquires an access token
handle_packet(#packet{type=login,
                      fields=#{email := Email,
                               password := SentPass,
                               perms := Permissions,
                               agent := #entity{type = agent, fields = Agent}}}, Main, Scope) ->
    % ensure proper connection state
    assert_state(Main, awaiting_login, Scope),
    % rate limiting
    {_, 1} = {{Scope, status_packet:make(rate_limiting, "Too many attempts. Please try again in a minute")}, sweet_main:ratelimit(Main, login)},
    % get the user and ensure they're the only one with this email (could be none)
    {ok, User} = cqerl:run_query(get(cassandra), #cql_query{
        statement = "SELECT salt, password, mfa_secret, id FROM users_by_email WHERE email=?",
        values    = [{email, Email}]
    }),
    {_, 1} = {{Scope, status_packet:make(login_error, "Invalid E-Mail", Seq)}, cqerl:size(User)},
    Row = cqerl:head(User),
    % check the password the user sent us
    Salt      = proplists:get_value(salt, Row),
    Password  = proplists:get_value(password, Row),
    MfaSecret = proplists:get_value(mfa_secret, Row),
    Id        = proplists:get_value(id, Row),
    {_, Password} = {{Scope, status_packet:make(login_error, "Invalid password", Seq)}, utils:hash_password(SentPass, Salt)},
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
handle_packet(#packet{type=mfa_secret,
                      fields=#{secret:=Token}}, Main, Scope) ->
    % ensure proper connection state
    {_, awaiting_mfa} = {{Scope, status_packet:make_invalid_state(awaiting_login, Seq)}, get(state)},
    % rate limiting
    {_, 1} = {{Scope, status_packet:make(rate_limiting, "Please try again in a minute", Seq)}, ratelimit:hit(login)},
    % verify 2FA token
    {_, true} = {{Scope, status_packet:make(login_error, "Invalid 2FA token", Seq)},
            yamka_auth:totp_verify(get(mfa_secret), Token)},
    put(mfa_secret, none),
    put(state, awaiting_login),
    access_token_packet:make(yamka_auth:create_token(get(mfa_perms), get(mfa_agent)), Seq);


%% signup packet (to create an account)
handle_packet(#packet{type=signup,
                      fields=#{email   :=EMail,
                               password:=SentPass,
                               name    :=Name,
                               agent:=#entity{type=agent, fields=Agent}}}, Main, Scope) ->
    % ensure proper connection state
    {_, awaiting_login} = {{Scope, status_packet:make_invalid_state(awaiting_login, Seq)}, get(state)},
    % check if the E-Mail is valid
    {_, true} = {{Scope, status_packet:make(signup_error, "Invalid E-Mail", Seq)},
        email:is_valid(EMail)},
    {_, true} = {{Scope, status_packet:make(signup_error, "Use a longer password", Seq)},
        length(SentPass) >= 6},
    {_, true} = {{Scope, status_packet:make(signup_error, "The name is too long or too short", Seq)},
        (length(Name) >= 3) and (length(Name) =< 64)},
    {_, false} = {{Scope, status_packet:make(signup_error, "E-Mail is already in use", Seq)},
        user_e:email_in_use(EMail)},
    Id = user_e:create(Name, EMail, SentPass),
    % create an agent or use an existing one
    AgentId = case Agent of
        #{id := Existing}              -> Existing;
        #{type := Type, name := AName} -> agent_e:create(Id, Type, AName)
    end,
    access_token_packet:make(yamka_auth:create_token(?ALL_PERMISSIONS_EXCEPT_BOT, AgentId), Seq);


%% access token packet (to identify the user and permissions)
handle_packet(#packet{type=access_token,
                      fields=#{token := Token}}, Main, Scope) ->
    % ensure proper connection state
    {_, awaiting_login} = {{Scope, status_packet:make_invalid_state(awaiting_login, Seq)}, get(state)},
    % get the token
    {_, {AgentId, Perms}} = {{Scope, status_packet:make(invalid_access_token, "Invalid token")}, yamka_auth:get_token(Token)},
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
handle_packet(#packet{type=entity_get,
                      fields=#{entities := Entities}}, Main, Scope) ->
    {_, normal} = {{Scope, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    entities_packet:make([entity:handle_get_request(R, {Scope, Seq}) || R <- Entities], Seq);


%% entity packet (to put a set of entities)
handle_packet(#packet{type=entities,
                      fields=#{entities := Entities}}, Main, Scope) ->
    {_, normal} = {{Scope, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    {_, false} = {{Scope, status_packet:make_excessive_data(Seq)}, entity:check_excessivity(Entities)},
    [entity:handle_entity(R, Seq, Scope) || R <- Entities];


%% file download request (to download a file)
handle_packet(#packet{type=file_download_request,
                      fields=#{id := Id}}, Main, Scope) ->
    {_, normal} = {{Scope, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    {_, true} = {{Scope, status_packet:make(invalid_id, "Unknown ID", Seq)}, file_storage:exists(Id)},
    file_storage:send_file(Id, Seq, {get(socket), get(protocol)}),
    none;


%% file data chunk (to upload a file)
handle_packet(#packet{type=file_data_chunk,
                      fields=#{data := Data}}, Main, Scope) ->
    {_, normal} = {{Scope, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    get(file_recv_pid) ! Data,
    none;


%% accept friend request
handle_packet(#packet{type=contacts_manage,
                      fields=#{type:=friend, action:=add, id:=Id}}, Main, Scope) ->
    {_, normal} = {{Scope, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    yamka_auth:assert_permission(edit_relationships, {Scope, Seq}),
    % check if that user has, in fact, sent a friend request
    #{pending_in := Requests} = user_e:get(get(id)),
    {_, true} = {{Scope, status_packet:make(contact_action_not_applicable, "This user has not issued a friend request", Seq)},
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
handle_packet(#packet{type=contacts_manage,
                      fields=#{type:=blocked, action:=add, id:=Id}}, Main, Scope) ->
    {_, normal} = {{Scope, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    yamka_auth:assert_permission(edit_relationships, {Scope, Seq}),
    % write to DB
    user_e:remove_dm_channel([Id, get(id)]),
    user_e:remove_friend(Id, get(id)),
    user_e:block(Id, get(id)),
    % broadcast changes
    icpc_broadcast_entity(Id,      #entity{type=user, fields=user_e:get(Id)},      [id, friends]),
    icpc_broadcast_entity(get(id), #entity{type=user, fields=user_e:get(get(id))}, [id, friends, blocked]),
    none;
%% decline friend request
handle_packet(#packet{type=contacts_manage,
                      fields=#{type:=pending_in, action:=remove, id:=Id}}, Main, Scope) ->
    {_, normal} = {{Scope, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    yamka_auth:assert_permission(edit_relationships, {Scope, Seq}),
    % write to DB
    user_e:decline_friend_rq(Id, get(id)),
    % broadcast changes
    icpc_broadcast_entity(Id,      #entity{type=user, fields=user_e:get(Id)},      [id, pending_out]),
    icpc_broadcast_entity(get(id), #entity{type=user, fields=user_e:get(get(id))}, [id, pending_in]),
    none;
%% cancel a friend request we sent
handle_packet(#packet{type=contacts_manage,
                      fields=#{type:=pending_out, action:=remove, id:=Id}}, Main, Scope) ->
    {_, normal} = {{Scope, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    yamka_auth:assert_permission(edit_relationships, {Scope, Seq}),
    % write to DB
    user_e:decline_friend_rq(get(id), Id),
    % broadcast changes
    icpc_broadcast_entity(get(id), #entity{type=user, fields=user_e:get(get(id))}, [id, pending_out]),
    icpc_broadcast_entity(Id,      #entity{type=user, fields=user_e:get(Id)},      [id, pending_in]),
    none;
%% remove friend
handle_packet(#packet{type=contacts_manage,
                      fields=#{type:=friend, action:=remove, id:=Id}}, Main, Scope) ->
    {_, normal} = {{Scope, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    yamka_auth:assert_permission(edit_relationships, {Scope, Seq}),
    % write to DB
    user_e:remove_friend(Id, get(id)),
    % broadcast changes
    icpc_broadcast_entity(get(id), #entity{type=user, fields=user_e:get(get(id))}, [id, friends]),
    icpc_broadcast_entity(Id,      #entity{type=user, fields=user_e:get(Id)},      [id, friends]),
    none;
%% unblock
handle_packet(#packet{type=contacts_manage,
                      fields=#{type:=blocked, action:=remove, id:=Id}}, Main, Scope) ->
    {_, normal} = {{Scope, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    yamka_auth:assert_permission(edit_relationships, {Scope, Seq}),
    % write to DB
    user_e:unblock(Id, get(id)),
    % broadcast changes
    icpc_broadcast_entity(get(id), #entity{type=user, fields=user_e:get(get(id))}, [id, blocked]),
    none;
%% leave group
handle_packet(#packet{type=contacts_manage,
                      fields=#{type:=group, action:=remove, id:=Id}}, Main, Scope) ->
    {_, normal} = {{Scope, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    yamka_auth:assert_permission(edit_relationships, {Scope, Seq}),
    % write to DB
    #{everyone_role := Everyone} = group_e:get(Id),
    role_e:remove(Everyone, get(id)),
    % broadcast changes
    icpc_broadcast_entity(get(id), #entity{type=user, fields=user_e:get(get(id))}, [id, groups]),
    none;
%% invalid request
handle_packet(#packet{type=contacts_manage}, Main, Scope) ->
    {_, normal} = {{Scope, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    yamka_auth:assert_permission(edit_relationships, {Scope, Seq}),
    status_packet:make(contact_action_not_applicable, "Invalid request (check type and target id)", Seq);


%% user search packet (send a friend request using their name and tag)
handle_packet(#packet{type=search,
                      fields=#{type:=user, name:=Name}}, Main, Scope) ->
    {_, normal} = {{Scope, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    yamka_auth:assert_permission(edit_relationships, {Scope, Seq}),
    {_, {ok, Id}} = {{Scope, status_packet:make(invalid_username, "Invalid username", Seq)},
        utils:safe_call(fun user_e:search/1, [Name], [{cassandra, get(cassandra)}])},
    % write and broadcast changes
    user_e:send_friend_rq(get(id), Id),
    icpc_broadcast_entity(get(id), #entity{type=user, fields=user_e:get(get(id))}, [pending_out]),
    icpc_broadcast_entity(Id,      #entity{type=user, fields=user_e:get(Id)},      [pending_in]),
    status_packet:make(friend_request_sent, "Friend request sent", Seq);


%% group member search
handle_packet(#packet{type=search,
                      fields=#{type:=group_member, name:=Name, ref:=Id}}, Main, Scope) ->
    {_, normal} = {{Scope, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    yamka_auth:assert_permission(see_groups, {Scope, Seq}),
    Users = group_e:find_users(Id, Name, 5),
    search_result_packet:make(Users, Seq);


%% group emoji search
handle_packet(#packet{type=search,
                      fields=#{type:=group_emoji, name:=Name, ref:=Id}}, Main, Scope) ->
    {_, normal} = {{Scope, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    yamka_auth:assert_permission(see_groups, {Scope, Seq}),
    Emoji = group_e:find_emoji(Id, Name, 10),
    search_result_packet:make(Emoji, Seq);


%% invite resolution packet (to get the group by one of its invites)
handle_packet(#packet{type=invite_resolve,
                      fields=#{code:=Code, add:=Add}}, Main, Scope) ->
    {_, normal} = {{Scope, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    yamka_auth:assert_permission(join_groups, {Scope, Seq}),
    {_, {ok, Id}} = {{Scope, status_packet:make(invalid_invite, "Invalid invite", Seq)},
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
handle_packet(#packet{type=voice_join,
                      fields=#{channel:=Chan, crypto:=Key}}, Main, Scope) ->
    {_, normal} = {{Scope, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    {_, 1} = {{Scope, status_packet:make(rate_limiting, "Rate limiting")}, ratelimit:hit(voice)},
    Session = tasty:create_session(Key, get(id), Chan),
    Server = tasty:server_name(),
    logging:log("Redirecting voice client to ~p", [Server]),
    voice_join_packet:make(Session, Server, Seq);


%% email confirmation packet
handle_packet(#packet{type=email_confirmation,
                      fields=#{code:=Code}}, Main, Scope) ->
    {_, normal} = {{Scope, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    {_, ok} = {{Scope, status_packet:make(invalid_confirmation_code,
                          "Invalid email address confirmation code", Seq)},
        user_e:finish_email_confirmation(get(id), Code)},
    entities_packet:make([#entity{type=user, fields=#{id => get(id), email_confirmed => true}}]);


%% password change packet
handle_packet(#packet{type=password_change,
                      fields=#{old_pass:=OldPass, mfa_code:=MfaCode, pass:=Pass}}, Main, Scope) ->
    {_, normal} = {{Scope, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    {_, true} = {{Scope, status_packet:make(invalid_credential, "Use a longer password", Seq)},
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
    {_, ExPassword} = {{Scope, status_packet:make(invalid_credential, "Invalid current password", Seq)},
        utils:hash_password(OldPass, Salt)},
    % check the 2FA code
    MfaCheckPassed = case MfaSecret of
        null -> true;
        _ -> yamka_auth:totp_verify(MfaSecret, MfaCode)
    end,
    {_, true} = {{Scope, status_packet:make(invalid_credential, "Invalid 2FA code", Seq)},
        MfaCheckPassed},
    % change the password if all checks passed
    NewHash = utils:hash_password(Pass, Salt),
    {ok, _} = cqerl:run_query(get(cassandra), #cql_query{
        statement = "UPDATE users SET password=? WHERE id=?",
        values    = [{id, get(id)}, {password, NewHash}]
    }),
    status_packet:make(password_changed, "Changed password successfully", Seq);


%% 2FA change packet
handle_packet(#packet{type=mfa_toggle,
                      fields=#{pass:=Pass, enable:=Enable}}, Main, Scope) ->
    {_, normal} = {{Scope, status_packet:make_invalid_state(normal, Seq)}, get(state)},
    % make sure the password is correct
    {_, true} = {{Scope, status_packet:make(invalid_credential, "Invalid password", Seq)},
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
handle_packet(#packet{type=ping,
                      fields=#{echo := Echo}}, _Main, _Scope) ->
    #packet{type = pong, reply = Seq, fields = #{echo => Echo}};

handle_packet(_, _, _) ->
    status_packet:make(unknown_packet, "Unknown packet type").

start(Main, ConnState, ProtoVer, Cassandra, Packet) ->
    put(cassandra, Cassandra),
    % thanks to José M at https://stackoverflow.com/a/65711977/8074626
    % for this cool match error handling technique
    Scope = make_ref(),
    Seq = Packet#packet.seq,

    % handle the packet and catch errors in doing so
    Result = try
        handle_packet(Packet, Main, Scope)
    of
        Value -> Value
    catch
        error:{badmatch, {{Scope, ErrResult}, _}} -> ErrResult
    end,

    case Result of
        stop ->
            sweet_main:stop(self());
        
        Packets when is_list(Packets) ->
            [sweet_main:send_packet(self(), P#packet{reply = Seq}) || P <- Packets];

        Packet when is_record(Packet, packet) ->
            sweet_main:send_packet(self(), Packet#packet{reply = Seq});

        _ -> ok
    end.