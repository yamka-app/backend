%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(sweet_handler).
-author("Yamka").
-license("MPL-2.0").
-description("Packet handler process for each packet").

-include("../packets/packet.hrl").
-include("../entities/entity.hrl").
-include_lib("cqerl/include/cqerl.hrl").

-export([start/4]).

%% asserts a connection state
assert_state(Wanted) ->
    {_, {Wanted, _}} = {{if_failed, status_packet:make_invalid_state(Wanted)}, sweet_main:get_state(get(main))}.


%% acquires an access token
handle_packet(#packet{type=login,
                      fields = #{email := Email,
                                 password := SentPass,
                                 perms := Permissions,
                                 agent := #entity{type = agent, fields = Agent}}}) ->
    assert_state(awaiting_login),

    % rate limiting
    {_, 1} = {{if_failed, status_packet:make(rate_limiting, "Too many attempts. Please try again in a minute")},
            sweet_main:ratelimit(get(main), login)},

    % get user 
    {_, #{salt := Salt, password := Password, mfa_secret := MfaSecret, id := Id}}
            = {{if_failed, status_packet:make(login_error, "Invalid E-Mail")},
                cassandra:select_one("users_by_email", #{email => Email})},
    
    % check password
    {_, Password} = {{if_failed, status_packet:make(login_error, "Invalid password")},
            utils:hash_password(SentPass, Salt)},

    % create an agent or use an existing one
    AgentId = case Agent of
        #{id := Existing}             -> Existing;
        #{type := Type, name := Name} -> agent_e:create(Id, Type, Name)
    end,

    % generate the token depending on whether the client has 2FA enabled or not
    case MfaSecret of
        null ->
            access_token_packet:make(yamka_auth:create_token(Permissions, AgentId));
        Secret ->
            sweet_main:switch_state(get(main), awaiting_mfa, {Id, AgentId, Permissions, Secret}),
            status_packet:make(mfa_required, "2FA is enabled on this account")
    end;


%% completes authentication when using 2FA
handle_packet(#packet{type=mfa_secret,
                      fields = #{secret := Token}}) ->
    assert_state(awaiting_mfa),

    % rate limiting
    {_, 1} = {{get(scope), status_packet:make(rate_limiting, "Too many attempts. Please try again in a minute")},
            sweet_main:ratelimit(get(main), login)},

    % verify sent token
    {_, {Id, Agent, Perms, Secret}} = sweet_main:get_state(get(main)),
    {_, true} = {{if_failed, status_packet:make(login_error, "Invalid 2FA token")},
            yamka_auth:totp_verify(Secret, Token)},

    % update state
    sweet_main:switch_state(get(main), awaiting_mfa, {}), % erase MFA state
    sweet_main:switch_state(get(main), awaiting_login),

    % generate and send token
    access_token_packet:make(yamka_auth:create_token(Perms, Agent));


%% creates an account
handle_packet(#packet{type=signup,
                      fields = #{email := EMail,
                                 password := SentPass,
                                 name := Name,
                                 agent := #entity{type = agent, fields = Agent}}}) ->
    assert_state(awaiting_login),

    % check fields
    {_, true} = {{if_failed, status_packet:make(signup_error, "Invalid E-Mail")},
            email:is_valid(EMail)},
    {_, true} = {{if_failed, status_packet:make(signup_error, "Use a longer password")},
            length(SentPass) >= 6},
    {_, true} = {{if_failed, status_packet:make(signup_error, "The name is too long or too short")},
            (length(Name) >= 3) and (length(Name) =< 64)},
    {_, false} = {{if_failed, status_packet:make(signup_error, "E-Mail is already in use")},
            user_e:email_in_use(EMail)},

    % create the user
    Id = user_e:create(Name, EMail, SentPass),

    % create an agent or use an existing one
    AgentId = case Agent of
        #{id := Existing}              -> Existing;
        #{type := Type, name := AName} -> agent_e:create(Id, Type, AName)
    end,
    access_token_packet:make(yamka_auth:create_token(?ALL_PERMISSIONS_EXCEPT_BOT, AgentId));


%% access token packet (to identify the user and permissions)
handle_packet(#packet{type=access_token,
                      fields=#{token := Token}}) ->
    assert_state(awaiting_login),

    % get token and agent
    {_, {AgentId, Perms}} = {{if_failed, status_packet:make(invalid_access_token, "Invalid token")},
            yamka_auth:get_token(Token)},
    #{owner := Id} = agent_e:get(AgentId),

    % save state, tell others and send an identity packet
    sweet_main:switch_state(get(main), normal, {Id, AgentId, Perms}),
    user_e:broadcast_status(Id),
    client_identity_packet:make(Id, AgentId);


%% requests entities
handle_packet(#packet{type = entity_get,
                      fields = #{entities := Entities}}) ->
    assert_state(normal),
    entities_packet:make([entity_handler:handle_get_request(R) || R <- Entities]);


%% puts entities
handle_packet(#packet{type = entities,
                      fields = #{entities := Entities}}) ->
    assert_state(normal),

    % check if the client sent too much data
    {_, false} = {{if_failed, status_packet:make_excessive_data()},
            entity:check_excessivity(Entities)},
    [entity_handler:handle_entity(R) || R <- Entities];


%% starts a file download
handle_packet(#packet{type = file_download_request,
                      fields = #{id := Id}}) ->
    assert_state(normal),

    {_, true} = {{if_failed, status_packet:make(invalid_id, "Unknown ID")},
            file_storage:exists(Id)},
    file_storage:send_file(Id, get(seq), get(main)),
    none;


%% writes a data chunk to the file that's being currently uploaded
handle_packet(#packet{type = file_data_chunk,
                      fields = #{data := Data}}) ->
    assert_state(normal),
    RecvPid = sweet_main:get_file_recv_pid(get(main)),
    RecvPid ! Data,
    none;


%% accept friend request
handle_packet(#packet{type = contacts_manage,
                      fields = #{type := friend, action := add, id := Id}}) ->
    assert_state(normal),
    yamka_auth:assert_permission(edit_relationships),

    % check if that user has, in fact, sent a friend request
    #{pending_in := Requests} = user_e:get(get(id), false),
    {_, true} = {{if_failed, status_packet:make(contact_action_not_applicable, "This user has not issued a friend request")},
            lists:member(Id, Requests)},
            
    % write to DB
    DmId = channel_e:create("DM", 0, null, true),
    user_e:add_dm_channel([Id, get(id)], DmId),
    user_e:accept_friend_rq(Id, get(id)),

    % broadcast the changes
    sweet_main:route_to_owners(get(main), Id,      [id, friends, pending_out]),
    sweet_main:route_to_owners(get(main), get(id), [id, friends, pending_in]),
    none;


%% block
handle_packet(#packet{type = contacts_manage,
                      fields = #{type := blocked, action := add, id := Id}}) ->
    assert_state(normal),
    yamka_auth:assert_permission(edit_relationships),
    
    % write to DB
    user_e:remove_dm_channel([Id, get(id)]),
    user_e:remove_friend(Id, get(id)),
    user_e:block(Id, get(id)),

    % broadcast changes
    sweet_main:route_to_owners(get(main), Id,      [id, firends]),
    sweet_main:route_to_owners(get(main), get(id), [id, friends, blocked]),
    none;


%% decline friend request
handle_packet(#packet{type=contacts_manage,
                      fields=#{type:=pending_in, action:=remove, id:=Id}}) ->
    assert_state(normal),
    yamka_auth:assert_permission(edit_relationships),

    % write to DB
    user_e:decline_friend_rq(Id, get(id)),

    % broadcast changes
    sweet_main:route_to_owners(get(main), Id,      [id, pending_out]),
    sweet_main:route_to_owners(get(main), get(id), [id, pending_in]),
    none;


%% cancel a friend request we sent
handle_packet(#packet{type=contacts_manage,
                      fields=#{type:=pending_out, action:=remove, id:=Id}}) ->
    assert_state(normal),
    yamka_auth:assert_permission(edit_relationships),

    % write to DB
    user_e:decline_friend_rq(get(id), Id),

    % broadcast changes
    sweet_main:route_to_owners(get(main), Id,      [id, pending_in]),
    sweet_main:route_to_owners(get(main), get(id), [id, pending_out]),
    none;


%% remove friend
handle_packet(#packet{type=contacts_manage,
                      fields=#{type:=friend, action:=remove, id:=Id}}) ->
    assert_state(normal),
    yamka_auth:assert_permission(edit_relationships),

    % write to DB
    user_e:remove_friend(Id, get(id)),

    % broadcast changes
    sweet_main:route_to_owners(get(main), Id,      [id, friends]),
    sweet_main:route_to_owners(get(main), get(id), [id, friends]),
    none;


%% unblock
handle_packet(#packet{type=contacts_manage,
                      fields=#{type:=blocked, action:=remove, id:=Id}}) ->
    assert_state(normal),
    yamka_auth:assert_permission(edit_relationships),

    % write to DB
    user_e:unblock(Id, get(id)),

    % broadcast changes
    sweet_main:route_to_owners(get(main), Id, [id, blocked]),
    none;


%% leave group
handle_packet(#packet{type=contacts_manage,
                      fields=#{type:=group, action:=remove, id:=Id}}) ->
    assert_state(normal),
    yamka_auth:assert_permission(edit_relationships),

    % write to DB
    #{everyone_role := Everyone} = group_e:get(Id),
    role_e:remove(Everyone, get(id)),
    
    % broadcast changes
    sweet_main:route_to_owners(get(main), Id, [id, groups]),
    none;

%% invalid request
handle_packet(#packet{type=contacts_manage}) ->
    assert_state(normal),
    yamka_auth:assert_permission(edit_relationships),
    status_packet:make(contact_action_not_applicable, "Invalid request (check type and target id)");


%% user search packet (send a friend request using their name and tag)
handle_packet(#packet{type=search,
                     fields=#{type:=user, name:=Name}}) ->
    assert_state(normal),
    yamka_auth:assert_permission(edit_relationships),
    {_, {ok, Id}} = {{if_failed, status_packet:make(invalid_username, "Invalid username")}, user_e:search(Name)},

    % write to DB
    user_e:send_friend_rq(get(id), Id),

    % broadcast changes
    sweet_main:route_to_owners(get(main), get(id), [id, pending_out]),
    sweet_main:route_to_owners(get(main), Id,      [id, pending_in]),
    status_packet:make(friend_request_sent, "Friend request sent");


%% group member search
handle_packet(#packet{type=search,
                      fields=#{type:=group_member, name:=Name, ref:=Id}}) ->
    assert_state(normal),
    yamka_auth:assert_permission(see_groups),
    Users = group_e:find_users(Id, Name, 5),
    search_result_packet:make(Users);


%% group emoji search
handle_packet(#packet{type=search,
                      fields=#{type:=group_emoji, name:=Name, ref:=Id}}) ->
    assert_state(normal),
    yamka_auth:assert_permission(see_groups),
    Emoji = group_e:find_emoji(Id, Name, 10),
    search_result_packet:make(Emoji);


%% invite resolution packet (to get the group by one of its invites)
handle_packet(#packet{type=invite_resolve,
                      fields=#{code:=Code, add:=Add}}) ->
    assert_state(normal),
    yamka_auth:assert_permission(join_groups),
    {_, {ok, Id}} = {{if_failed, status_packet:make(invalid_invite, "Invalid invite")},
        group_e:resolve_invite(Code)},
    % add user to the group if asked to
    case Add of
        false -> ok;
        true ->
            #{name := SelfName} = user_e:get(get(id), false),
            #{everyone_role := Everyone} = group_e:get(Id),
            role_e:add(Everyone, get(id)),
            group_e:cache_user_name(Id, get(id), SelfName),
            sweet_main:route_to_owners(get(main), get(id), [id, groups])
    end,
    entities_packet:make([#entity{type=group, fields=group_e:get(Id, false)}]);


%% voice join packet
handle_packet(#packet{type=voice_join,
                      fields=#{channel:=Chan, crypto:=Key}}) ->
    assert_state(normal),
    yamka_auth:assert_permission(join_groups),
    {_, {ok, 1}} = {{if_failed, status_packet:make(rate_limiting, "Rate limiting")}, sweet_main:ratelimit(get(main), voice)},
    Session = tasty:create_session(Key, get(id), Chan),
    Server = tasty:server_name(),
    lager:info("Redirecting voice client to ~p", [Server]),
    voice_join_packet:make(Session, Server);


%% email confirmation packet
handle_packet(#packet{type=email_confirmation,
                      fields=#{code:=Code}}) ->
    assert_state(normal),
    {_, ok} = {{if_failed, status_packet:make(invalid_confirmation_code,
                          "Invalid email address confirmation code")},
        user_e:finish_email_confirmation(get(id), Code)},
    entities_packet:make([#entity{type=user, fields=#{id => get(id), email_confirmed => true}}]);


%% password change packet
handle_packet(#packet{type=password_change,
                      fields=#{old_pass:=OldPass, mfa_code:=MfaCode, pass:=Pass}}) ->
    assert_state(normal),
    {_, true} = {{if_failed, status_packet:make(invalid_credential, "Use a longer password")},
        length(Pass) >= 6},
    % make sure the old password is correct
    {_, true} = {{if_failed, status_packet:make(invalid_credential, "Invalid old password")},
        yamka_auth:pass_verify(get(id), OldPass)},
    % check the 2FA code
    {ok, Rows} = cqerl:run_query(get(cassandra), #cql_query{
        statement = "SELECT salt, mfa_secret FROM users WHERE id=?",
        values    = [{id, get(id)}]
    }),
    [{salt, Salt}, {mfa_secret, MfaSecret}] = cqerl:head(Rows),
    MfaCheckPassed = case MfaSecret of
        null -> true;
        _ -> yamka_auth:totp_verify(MfaSecret, MfaCode)
    end,
    {_, true} = {{if_failed, status_packet:make(invalid_credential, "Invalid 2FA code")},
        MfaCheckPassed},
    % change the password if all checks passed
    NewHash = utils:hash_password(Pass, Salt),
    user_e:update(get(id), #{password => NewHash}),
    status_packet:make(password_changed, "Changed password successfully");


%% 2FA change packet
handle_packet(#packet{type=mfa_toggle,
                      fields=#{pass:=Pass, enable:=Enable}}) ->
    assert_state(normal),
    % make sure the password is correct
    {_, true} = {{if_failed, status_packet:make(invalid_credential, "Invalid password")},
        yamka_auth:pass_verify(get(id), Pass)},
    % broadcast the changes
    sweet_main:route_entity(get(main), {aware, {user, get(id)}}, #entity{type=user,
        fields=#{id => get(id), mfa_enabled => Enable}}, [id, mfa_enabled]),
    % reply with nothing or with our newly generated secret
    if
        Enable ->
            Secret = yamka_auth:totp_secret(),
            user_e:update(get(id), #{mfa_secret => Secret}),
            mfa_secret_packet:make(Secret);
        true ->
            user_e:update(get(id), #{mfa_secret => null}),
            status_packet:make(mfa_toggled, "2FA disabled")
    end;

%% ping packet (to signal to the server that the connection is alive)
handle_packet(#packet{type=ping,
                      fields=#{echo := Echo}}) ->
    #packet{type = pong, fields = #{echo => Echo}};

handle_packet(_) ->
    status_packet:make(unknown_packet, "Unknown packet type").

start(Main, Id, Cassandra, Packet) ->
    % put some things into the process dictionary
    % hey, pure functional programming can be quite cool... sometimes...
    % but I like Erlang for having an option to have something like this
    % for things that have to be referenced five function calls deep
    put(cassandra, Cassandra),
    put(main, Main),
    put(seq, Packet#packet.seq),
    put(id, Id),

    % thanks to José M at https://stackoverflow.com/a/65711977/8074626
    % for this cool match error handling technique
    Result = try
        handle_packet(Packet)
    of
        Value -> Value
    catch
        error:{badmatch, {{if_failed, ErrResult}, _}} -> ErrResult
    end,

    case Result of
        stop ->
            sweet_main:stop(Main);
        
        Packets when is_list(Packets) ->
            [if is_record(P, packet) ->
                    sweet_main:send_packet(Main, P#packet{reply=get(seq)});
                true -> ok
             end || P <- Packets];

        #packet{}=RepPacket ->
            sweet_main:send_packet(Main, RepPacket#packet{reply=get(seq)});

        _ -> ok
    end.