%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(entity_handler).
-author("Yamka").
-license("MPL-2.0").

-include("entity.hrl").

-export([handle_get_request/1, handle_entity/1]).

%% updates the logged in user
handle_entity(#entity{type=user, fields=#{id:=0} = F}) ->
    yamka_auth:assert_permission(edit_profile),

    % only allow allowed fields (wow thx captain obvious)
    BroadcastFields = [name, status, status_text, ava_file, fav_color],
    AllowedFields = maps:filter(fun(K, _) -> lists:member(K, [email|BroadcastFields]) end, F),
    case maps:size(AllowedFields) of
        0 -> ok;
        _ ->
            % change DB record
            user_e:update(get(id), AllowedFields),
            % broadcast changes
            sweet_main:route_to_aware(get(main), {user, get(id)}, [id] ++ maps:keys(AllowedFields))
    end,

    % if the email was changed, un-confirm it and send a confirmation request to the user
    EmailChanged = maps:is_key(email, F),
    if
        EmailChanged ->
            Email = maps:get(email, F),
            user_e:update(get(id), #{email => Email, email_confirmed => false}),
            user_e:start_email_confirmation(get(id), Email),
            sweet_main:route_to_owners(get(main), get(id), [id, email, email_confirmed]);
        true -> ok
    end,

    % if the agent list was changed, calculate the diff and yank the removed ones
    % (along with their access tokens!)
    AgentsChanged = maps:is_key(agents, F),
    if
        AgentsChanged ->
            OldAgents = agent_e:get_by_user(get(id)),
            {_, RemovedAgents} = utils:list_diff(maps:get(agents, F), OldAgents),
            [agent_e:delete(A)          || A <- RemovedAgents],
            [yamka_auth:revoke_agent(A) || A <- RemovedAgents],
            % send updates
            sweet_main:route_to_owners(get(main), #entity{type=user, fields=
                #{id => get(id), agents => agent_e:get_by_user(get(id))}});
        true -> ok
    end,
    none;


%% sets a note
handle_entity(#entity{type=user, fields=#{id:=Id, note:=Note}}) ->
    user_e:set_note(Id, get(id), Note),
    none;

%% puts a file
handle_entity(#entity{type=file, fields=Fields=#{name:=Name, length:=Length}}) ->
    {_, nopid} = {{if_failed, status_packet:make(one_upload_only,
            "Only one concurrent upload is allowed")}, sweet_main:get_file_recv_pid(get(main))},
    MaxSize = file_storage:max_size(),
    EmojiName = maps:get(emoji_name, Fields, ""),
    if
        Length =< MaxSize -> 
            sweet_main:set_file_recv_pid(get(main), file_storage:recv_file(get(seq),
                {get(main), get(cassandra)},
                {Length, Name, EmojiName})),
            none;
        true ->
            status_packet:make(file_too_large, "Max file size is " ++ file_storage:max_size_text())
    end;


%% renames an emoji
handle_entity(#entity{type=file, fields=#{id:=Id, emoji_name:=Name}}) ->
    #{emoji_group := Group} = file_e:get(Id),
    group_e:assert_permission(Group, edit_emoji),
    % write to DB
    file_e:update(Id, #{emoji_name => Name}),
    % broadcast
    sweet_awareness:add({file, Id}, get(main)),
    sweet_main:route_to_aware(get(main), {file, Id}, [id, emoji_name]),
    none;


%% (re-)sets the typing status
handle_entity(#entity{type=channel, fields=#{id:=Id, typing:=[0]}}) ->
    sweet_main:route_to_aware(get(main), {channel, Id}, [id, typing]),
    none;
handle_entity(#entity{type=channel, fields=#{id:=Id, typing:=[]}}) ->
    sweet_main:route_to_aware(get(main), {channel, Id}, [id, typing]),
    none;


%% sets the unread message
handle_entity(#entity{type=channel, fields=#{id:=Id, first_unread:=FirstUnread}}) ->
    % get the message and its LCID
    #{lcid := Lcid} = message_e:get(FirstUnread),
    channel_e:set_unread(Id, get(id), {Lcid, FirstUnread}),
    none;
%% marks the channel as read
handle_entity(#entity{type=channel, fields=#{id:=Id, unread:=0}}) ->
    % get the last message and its LCID
    case channel_e:get_messages(Id, 9223372036854775807, 1, down) of
        [] -> ok;
        [LastMsg] ->
            #{lcid := Lcid} = message_e:get(LastMsg),
            channel_e:set_unread(Id, get(id), {Lcid, LastMsg}),
            channel_e:forget_mentions(Id, get(id))
    end,
    none;


%% creates a channel
handle_entity(#entity{type=channel, fields=#{id:=0, group:=Group, name:=Name}}) ->
    yamka_auth:assert_permission(edit_groups),
    group_e:assert_permission(Group, edit_channels),

    channel_e:create(Name, Group, [], false),
    sweet_main:route_to_aware(get(main), {group, Group}, [id, channels]),
    none;


%% deletes a channel
handle_entity(#entity{type=channel, fields=#{id:=Id, group:=0}}) ->
    yamka_auth:assert_permission(edit_groups),
    #{group := Group} = channel_e:get(Id),
    group_e:assert_permission(Group, edit_channels),

    channel_e:delete(Id),
    sweet_main:route_to_aware(get(main), {group, Group}, [id, channels]),
    none;


%% modifies a channel
handle_entity(#entity{type=channel, fields=Fields=#{id:=Id}}) ->
    yamka_auth:assert_permission(edit_groups),
    #{group := Group} = channel_e:get(Id),
    group_e:assert_permission(Group, edit_channels),
    
    channel_e:update(Id, maps:filter(fun(K, _) -> (K =:= name) or (K =:= voice) end, Fields)),
    sweet_main:route_to_aware(get(main), {channel, Id}),
    none;


%% sends a group message
handle_entity(#entity{type=message,       fields=#{id:=0, channel:=Channel, latest:=
              #entity{type=message_state, fields=#{sections:=Sections}}}}) ->
    yamka_auth:assert_permission(send_group_messages),
    % check permissions
    #{group := Group} = channel_e:get(Channel),
    % group messages should be sent unencrypted
    {_, true} = {{if_failed, status_packet:make(invalid_request, "\"sections\" field in direct message")}, Group > 0},
    group_e:assert_permission(Group, send_messages),

    % filter sections
    Filtered = message_state_e:filter_sections(Sections),
    % check section count
    {_, true} = {{if_failed, status_packet:make(invalid_request, "Message should have at least 1 section")}, length(Filtered) >= 1},
    % parse mentions
    Mentions = message_state_e:parse_mentions(Filtered),
    % create entities
    {MsgId, _} = message_e:create(Channel, get(id)),
    message_state_e:create(MsgId, Filtered),
    % register mentions
    [channel_e:add_mention(Channel, User, MsgId) || User <- Mentions],
    % broadcast the message
    sweet_main:route_entity(get(main), {aware, {channel, Channel}}, message_e:get_full_record(MsgId)),
    none;


%% sends a direct message
handle_entity(#entity{type=message,       fields=#{channel:=Channel, latest:=
              #entity{type=message_state, fields=#{encrypted:=Encrypted}}}}) ->
    yamka_auth:assert_permission(send_direct_messages),
    % check permissions
    #{group := Group} = channel_e:get(Channel),
    % DMs should be sent encrypted
    {_, true} = {{if_failed, status_packet:make(invalid_request, "\"encrypted\" field in group message")}, Group =:= 0},

    % create entities
    {MsgId, _} = message_e:create(Channel, get(id)),
    message_state_e:create(MsgId, Encrypted),
    % broadcast the message
    sweet_main:route_entity(get(main), {aware, {channel, Channel}}, message_e:get_full_record(MsgId)),
    none;


%% edits a group message
handle_entity(#entity{type=message,       fields=#{id:=Id, latest:=
              #entity{type=message_state, fields=#{sections:=Sections}}}}) ->
    yamka_auth:assert_permission(send_group_messages),
    #{channel := Channel} = Existing = message_e:get(Id),
    #{group := Group} = channel_e:get(Channel),
    {_, true} = {{if_failed, status_packet:make(invalid_request, "\"sections\" field in direct message")}, Group > 0},
    SelfSent = maps:get(sender, Existing) =:= get(id),
    {_, true} = {{if_failed, status_packet:make(permission_denied, "This message was sent by another user")},
        SelfSent},

    message_state_e:create(Id, message_state_e:filter_sections(Sections)),
    sweet_main:route_entity(get(main), {aware, {channel, Channel}}, message_e:get_full_record(Id)),
    none;


%% edits a direct message
handle_entity(#entity{type=message,       fields=#{id:=Id, latest:=
              #entity{type=message_state, fields=#{encrypted:=Encrypted}}}}) ->
    yamka_auth:assert_permission(send_direct_messages),
    #{channel := Channel} = Existing = message_e:get(Id),
    #{group := Group} = channel_e:get(Channel),
    {_, true} = {{if_failed, status_packet:make(invalid_request, "\"encrypted\" field in group message")}, Group =:= 0},
    SelfSent = maps:get(sender, Existing) =:= get(id),
    {_, true} = {{if_failed, status_packet:make(permission_denied, "This message was sent by another user")},
        SelfSent},

    message_state_e:create(Id, Encrypted),
    sweet_main:route_entity(get(main), {aware, {channel, Channel}}, message_e:get_full_record(Id)),
    none;


%% deletes a message
handle_entity(#entity{type=message, fields=#{id:=Id, sender:=0}}) ->
    #{channel := Channel, sender := Sender} = message_e:get(Id),
    #{group := Group, lcid := Lcid} = channel_e:get(Channel),
    {_, true} = {{if_failed, status_packet:make(permission_denied, "This message was sent by another user")},
        Sender =:= get(id)},
    #{group := Group} = channel_e:get(Channel),
    yamka_auth:assert_permission(if
        Group =/= 0 ->
            group_e:assert_permission(Group, delete_others_messages),
            delete_group_messages;
        true -> delete_direct_messages
    end),
    % decrease the LCID
    channel_e:update(Channel, #{lcid => Lcid - 1}),
    message_e:delete(Id),
    sweet_main:route_entity(get(main), {aware, {channel, Channel}}, #entity{type=message,
        fields=#{channel => Channel, id => Id, sender => 0}}),
    none;


%% creates a group
handle_entity(#entity{type=group, fields=#{id:=0, name:=Name}}) ->
    yamka_auth:assert_permission(create_groups),
    #{name := Username} = user_e:get(get(id), false),
    {Id, Everyone} = group_e:create(Name, get(id)),
    role_e:add(Everyone, get(id)),
    group_e:cache_user_name(Id, get(id), Username),
    sweet_main:route_to_owners(get(main), get(id), [id, groups]),
    none;


%% deletes a group
handle_entity(#entity{type=group, fields=#{id:=Id, owner:=0}}) ->
    yamka_auth:assert_permission(delete_groups),
    #{owner := Owner,
      channels := Channels,
      roles := Roles,
      invites := Invites,
      everyone_role := Everyone} = group_e:get(Id),
    {_, Owner} = {{if_failed, status_packet:make(permission_denied, "Only the owner can do this")}, get(id)},

    lists:foreach(fun(R) -> group_e:remove_invite(Id, R) end, Invites),
    group_e:delete(Id),
    lists:foreach(fun channel_e:delete/1, Channels),
    lists:foreach(fun role_e:nuke/1, lists:delete(Everyone, Roles)),
    role_e:nuke(Everyone, true),
    sweet_main:route_entity(get(main), {aware, {group, Id}}, #entity{type=group, fields=#{id => Id, owner => 0}}),
    none;


%% manages invites
handle_entity(#entity{type=group, fields=#{id:=Id, invites:=Invites}}) ->
    yamka_auth:assert_permission(edit_groups),
    group_e:assert_permission(Id, edit_invites),

    #{invites := ExInvites} = group_e:get(Id),
    {Added, Removed} = utils:list_diff(Invites, ExInvites),
    lists:foreach(fun(_) -> group_e:add_invite(Id) end, lists:seq(1, length(Added))),
    lists:foreach(fun(I) -> group_e:remove_invite(Id, I) end, Removed),
    #{invites := NewInvites} = group_e:get(Id),
    entities_packet:make([#entity{type=group, fields=#{id => Id, invites => NewInvites}}]);


%% manages emoji
handle_entity(#entity{type=group, fields=#{id:=Id, emoji:=Emoji}}) ->
    yamka_auth:assert_permission(edit_groups),
    group_e:assert_permission(Id, edit_emoji),

    #{emoji := ExEmoji} = group_e:get(Id),
    {Added, Removed} = utils:list_diff(Emoji, ExEmoji),
    lists:foreach(fun(I) ->
        file_e:update(I, #{emoji_group => Id})
    end, Added),
    % don't delete the file, just deasscoiate it
    lists:foreach(fun(I) -> file_e:update(I, #{emoji_group => null}) end, Removed),
    #{emoji := NewEmoji} = group_e:get(Id),
    entities_packet:make([#entity{type=group, fields=#{id => Id, emoji => NewEmoji}}]);


%% creates a poll
handle_entity(#entity{type=poll, fields=#{id:=0, options:=Options}}) ->
    yamka_auth:assert_permission(create_polls),

    {_, false} = {{if_failed, status_packet:make(poll_error, "Zero or too many options")},
        (length(Options) > 10) or (length(Options) == 0)},
    Id = poll_e:create(Options),
    sweet_awareness:add({poll, Id}, get(main)),
    entities_packet:make([#entity{type=poll, fields=#{
        id => Id, options => Options, total_votes => 0,
        option_votes => [0 || _ <- Options]}}]);


%% votes in a poll
handle_entity(#entity{type=poll, fields=#{id:=Id, self_vote:=Option}}) ->
    yamka_auth:assert_permission(vote_in_polls),

    {_, {error, novote}} = {{if_failed, status_packet:make(poll_error, "Already voted")}, poll_e:get_vote(Id, get(id))},
    {_, ok} = {{if_failed, status_packet:make(poll_error, "Invalid option")}, poll_e:vote(Id, get(id), Option)},
    sweet_main:route_to_aware(get(main), {poll, Id}, [id, total_votes, option_votes]),
    none;


%% publishes keys
handle_entity(#entity{type=pkey, fields=#{type:=id_sign, key:=KeyBin}}) ->
    % see if there's an identity key already
    case pkey_e:get_by_user(get(id), id_sign) of
        [] -> ok;
        [Existing] ->
            % TODO: SCREAM OUT LOUD TO EVERY USER THIS ONE SHARES A DM CHANNEL WITH
            pkey_e:delete(Existing)
    end,
    pkey_e:create(get(id), id_sign, KeyBin, null),
    none;
handle_entity(#entity{type=pkey, fields=#{type:=Type, key:=KeyBin, signature:=SignBin}})
            when Type =:= identity; Type =:= prekey ->
    % TODO: it would be a good idea to check the signature
    % see if there's a key already
    case pkey_e:get_by_user(get(id), Type) of
        [] -> ok;
        [Existing] -> pkey_e:delete(Existing)
    end,
    pkey_e:create(get(id), Type, KeyBin, SignBin),
    none;
handle_entity(#entity{type=pkey, fields=#{type:=otprekey, key:=KeyBin, signature:=SignBin}}) ->
    % TODO: again, it would be a good idea to check the signature
    Count = pkey_e:count(get(id), otprekey),
    {_, false} = {{if_failed, status_packet:make(key_error, "Reached the limit of 256 one-time prekeys")}, Count >= 256},
    pkey_e:create(get(id), otprekey, KeyBin, SignBin),
    none;


%% handle illegal requests
handle_entity(#entity{}) ->
    status_packet:make(invalid_request, "Illegal EntityPut request (check type and id)").





%% gets a user
handle_get_request(#entity_get_rq{type=user, id=Id, pagination=none, context=none, key=none}) ->
    yamka_auth:assert_permission(see_profile),
    Self = user_e:get(get(id)),
    IsSelf = Id == get(id),
    Online = user_e:online(Id),
    Unfiltered = user_e:get(Id),

    OtpHashes = if
        IsSelf ->
            #{otp_hashes => [pkey_e:fingerprint(KeyId)
                || KeyId <- pkey_e:get_by_user(get(id), otprekey)]};
        true -> #{}
    end,

    Note = maps:merge(OtpHashes, case user_e:get_note(Id, get(id)) of
        nonote -> #{};
        Str    -> #{note => Str}
    end),

    Dm = maps:merge(Note, case channel_e:get_dm([get(id), Id]) of
        nodm -> #{};
        DmId -> #{dm_channel => DmId}
    end),

    Mfa = maps:merge(Dm, #{mfa_enabled => maps:get(mfa_secret, Unfiltered) =/= null}),
    Agents = maps:merge(Mfa, if IsSelf -> #{agents => agent_e:get_by_user(Id)}; true -> #{} end),
    
    FilteredFields = maps:filter(fun(K, _) ->
        case K of
            id              -> true;
            email           -> IsSelf;
            email_confirmed -> IsSelf;
            mfa_enabled     -> IsSelf;
            name            -> true;
            tag             -> true;
            fav_color       -> true;
            status          -> true;
            status_text     -> true;
            ava_file        -> true;
            friends         -> yamka_auth:has_permission(see_relationships);
            blocked         -> yamka_auth:has_permission(see_relationships) and IsSelf;
            pending_in      -> yamka_auth:has_permission(see_relationships) and IsSelf;
            pending_out     -> yamka_auth:has_permission(see_relationships) and IsSelf;
            dm_channel      -> yamka_auth:has_permission(see_direct_messages);
            groups          -> yamka_auth:has_permission(see_groups);
            badges          -> true;
            bot_owner       -> true;
            note            -> not IsSelf;
            _ -> false
        end
    end, maps:map(fun(K, V) ->
            case K of
                status -> if
                        (V /= offline) and not Online -> offline;
                        true -> V
                    end;
                groups  -> utils:intersect_lists([V, maps:get(groups,  Self)]);
                friends -> utils:intersect_lists([V, maps:get(friends, Self)]);
                _ -> V
            end
        end, Unfiltered)),
    sweet_awareness:add({user, Id}, get(main)),
    #entity{type=user, fields=maps:merge(FilteredFields, Agents)};


%% gets a user in context of a group
handle_get_request(#entity_get_rq{type=user, id=Id, pagination=none, context=
        #entity_context{type=group, id=_Group}, key=none}) ->
    % TODO
    #entity{type=user, fields=#{id => Id, color => 0}};


%% gets user's keys
handle_get_request(#entity_get_rq{type=user, id=Id, pagination=none, context=none, key=KeyType}) ->
    yamka_auth:assert_permission(fetch_keys),

    Keys = pkey_e:get_by_user(Id, KeyType),
    {_, true} = {{if_failed, status_packet:make(key_error, "No key(s) of type " ++ atom_to_list(KeyType))},
        length(Keys) > 0},
    % delete the key if it's a one-time prekey
    OneTime = KeyType =:= otprekey,
    if
        length(Keys) =:= 0 -> #entity{type=user, fields=#{}};
        true ->
            Key = lists:nth(1, Keys),
            KeyObj = pkey_e:get(Key),
            if
                OneTime ->
                    {_, true} = {{if_failed, status_packet:make(key_error, "One-time prekey rate limiting")},
                        pkey_e:check_rate_limit(Id, get(id))},
                    pkey_e:limit_rate(Id, get(id)),
                    pkey_e:delete(Key);
                true -> ok
            end,
            #entity{type=user, fields=#{id => Id, KeyType =>
                #entity{type=pkey, fields=KeyObj}}}
    end;


%% gets a file
handle_get_request(#entity_get_rq{type=file, id=Id, pagination=none, context=none, key=none}) ->
    sweet_awareness:add({file, Id}, get(main)),
    % there are no permission restrictions on file accesses
    #entity{type=file, fields=file_e:get(Id)};


%% gets a channel
handle_get_request(#entity_get_rq{type=channel, id=Id, pagination=none, context=none, key=none}) ->
    Unfiltered = channel_e:get(Id),
    {UnreadLcid, UnreadId} = channel_e:get_unread(Id, get(id)),
    Mentions = channel_e:get_mentions(Id, get(id)),
    Filtered = maps:filter(fun(K, _) -> K =/= perms end, Unfiltered),
    UnreadCnt = maps:get(lcid, Unfiltered) - UnreadLcid,
    AddMap = case UnreadCnt of
            N when N =< 0 -> #{unread => 0};
            _ -> #{unread => UnreadCnt, first_unread => UnreadId}
        end,
    sweet_awareness:add({channel, Id}, get(main)),
    #entity{type=channel, fields=maps:merge(
        maps:merge(Filtered, AddMap),
        #{typing => [], mentions => Mentions})};


%% gets channel messages
handle_get_request(#entity_get_rq{type=channel, id=Id, pagination=#entity_pagination{
        field=4, dir=Dir, from=From, cnt=Cnt}, context=none, key=none}) ->
    #{group := Group} = channel_e:get(Id),
    yamka_auth:assert_permission(if
        Group =/= 0 ->
            group_e:assert_permission(Group, read_messages),
            read_group_message_history;
        true -> read_direct_message_history
    end),
    #entity{type=channel, fields=#{id => Id, messages => channel_e:get_messages(Id, From, Cnt, Dir)}};


%% gets a message by id
handle_get_request(#entity_get_rq{type=message, id=Id, pagination=none, context=none, key=none}) ->
    Filtered = maps:filter(fun(K, _) -> K /= lcid end, message_e:get(Id)),
    StateMap = #{states => message_e:get_states(Id), latest => #entity{type=message_state, fields=
        message_state_e:get(message_e:get_latest_state(Id))}},
    sweet_awareness:add({message, Id}, get(main)),
    #entity{type=message, fields=maps:merge(Filtered, StateMap)};


%% gets a message state by id
handle_get_request(#entity_get_rq{type=message_state, id=Id, pagination=none, context=none, key=none}) ->
    #entity{type=message_state, fields=message_state_e:get(Id)};


%% gets a group by id
handle_get_request(#entity_get_rq{type=group, id=Id, pagination=none, context=none, key=none}) ->
    sweet_awareness:add({group, Id}, get(main)),
    #entity{type=group, fields=group_e:get(Id)};


%% gets a role by id
handle_get_request(#entity_get_rq{type=role, id=Id, pagination=none, context=none, key=none}) ->
    sweet_awareness:add({role, Id}, get(main)),
    #entity{type=role, fields=role_e:get(Id)};


%% gets role members
handle_get_request(#entity_get_rq{type=role, id=Id, pagination=#entity_pagination{
        field=6, dir=Dir, from=From, cnt=Cnt}, context=none, key=none}) ->
    #{group := Group} = role_e:get(Id),
    group_e:assert_permission(Group, see_members),
    #entity{type=role, fields=#{id => Id, members => role_e:get_members(Id, From, Cnt, Dir)}};


%% gets a poll by id
handle_get_request(#entity_get_rq{type=poll, id=Id, pagination=none, context=none, key=none}) ->
    Main = poll_e:get(Id),
    Fields = case poll_e:get_vote(Id, get(id)) of
        {error, novote} -> Main;
        {ok, Option}    -> maps:merge(Main, #{self_vote => Option})
    end,
    sweet_awareness:add({poll, Id}, get(main)),
    #entity{type=poll, fields=Fields};


%% gets an agent by id
handle_get_request(#entity_get_rq{type=agent, id=Id, pagination=none, context=none, key=none}) ->
    Online = #{online => agent_e:online(Id)},
    #entity{type=agent, fields=maps:merge(agent_e:get(Id), Online)};


%% gets a key by id
handle_get_request(#entity_get_rq{type=key, id=Id, pagination=none, context=none, key=none}) ->
    #entity{type=key, fields=pkey_e:get(Id)};


%% handle illegal requests
handle_get_request(#entity_get_rq{}) ->
    status_packet:make(invalid_request, "Illegal EntityGet request (check type and modifiers)").