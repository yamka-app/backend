%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(entity_handler).
-author("Yamka").
-license("MPL-2.0").

-include("../packets/packet.hrl").
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
            Entity = #entity{type=user, fields=maps:merge(AllowedFields, #{id => get(id)})},
            sweet_main:route_to_aware(get(main), {user, get(id)}, [id] ++ maps:keys(AllowedFields))
    end,

    % if the email was changed, un-confirm it and send a confirmation request to the user
    EmailChanged = maps:is_key(email, F),
    if
        EmailChanged ->
            Email = maps:get(email, F),
            user_e:update(get(id), #{email => Email, email_confirmed => false}),
            user_e:start_email_confirmation(get(id), Email),
            NewEntity = #entity{type=user, fields=#{
                id => get(id),
                email => Email,
                email_confirmed => false}},
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
            % construct the updated list of agents
            NewAgents = lists:foldl(fun(A, Acc) -> case
                lists:member(A, RemovedAgents) of true -> Acc;
                _ -> [A|Acc] end
            end, [], OldAgents),
            case RemovedAgents of
                [] -> ok;
                _ -> sweet_main:route_to_aware(get(main), {user, get(id)}, [id, agents])
            end;
        true -> ok
    end,
    none;


%% sets a note
handle_entity(#entity{type=user, fields=#{id:=Id, note:=Note}}) ->
    user_e:set_note(Id, get(id), Note),
    none;

%% puts a file
handle_entity(#entity{type=file, fields=Fields=#{name:=Name, length:=Length}}) ->
    {_, {ok, nopid}} = {{if_failed, status_packet:make(one_upload_only,
            "Only one concurrent upload is allowed")}, sweet_main:get_file_recv_pid(get(main))},
    MaxSize = file_storage:max_size(),
    EmojiName = maps:get(emoji_name, Fields, ""),
    if
        Length =< MaxSize -> 
            sweet_main:set_file_recv_pid(get(main), file_storage:recv_file(Seq,
                {get(socket), get(protocol), self(), get(cassandra)},
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
    ets:insert(file_awareness, {Id, {get(id), self()}}),
    sweet_main:route_to_aware(get(main), {file, Id}, [id, emoji_name]),
    none;


%% (re-)sets the typing status
handle_entity(#entity{type=channel, fields=#{id:=Id, typing:=[0]}}) ->
    channel_e:set_typing(Id, get(id)),
    sweet_main:route_to_aware(get(main), {channel, Id}, [id, typing]),
    none;
handle_entity(#entity{type=channel, fields=#{id:=Id, typing:=[]}}) ->
    channel_e:reset_typing(Id, get(id)),
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
handle_entity(#entity{type=channel, fields=Fields=#{id:=Id}}, Seq, Ref) ->
    yamka_auth:assert_permission(edit_groups, {Ref, Seq}),
    #{group := Group} = channel_e:get(Id),
    group_e:assert_permission(Group, edit_channels, {Ref, Seq}),
    
    channel_e:update(Id, maps:filter(fun(K, _) -> (K =:= name) or (K =:= voice) end, Fields)),
    client:icpc_broadcast_to_aware(chan_awareness,
        #entity{type=channel, fields=Fields}, maps:keys(Fields)),
    none;


%% sends a group message
handle_entity(M=#entity{type=message,       fields=#{id:=0, channel:=Channel, latest:=
              L=#entity{type=message_state, fields=#{sections:=Sections}}}}, Seq, Ref) ->
    % check permissions
    #{group := Group} = channel_e:get(Channel),
    % group messages should be sent unencrypted
    {_, true} = {{Ref, status_packet:make(invalid_request, "\"sections\" field in direct message", Seq)}, Group > 0},
    group_e:assert_permission(Group, send_messages, {Ref, Seq}),
    yamka_auth:assert_permission(send_group_messages, {Ref, Seq}),

    % filter sections
    Filtered = message_state_e:filter_sections(Sections),
    % check section count
    {_, true} = {{Ref, status_packet:make(invalid_request, "Message should have at least 1 section", Seq)}, length(Filtered) >= 1},
    % parse mentions
    Mentions = message_state_e:parse_mentions(Filtered),
    % create entities
    {MsgId, _} = message_e:create(Channel, get(id)),
    StateId = message_state_e:create(MsgId, Filtered),
    % register mentions
    [channel_e:add_mention(Channel, User, MsgId) || User <- Mentions],
    % broadcast the message
    client:icpc_broadcast_to_aware(chan_awareness, Channel,
        M#entity{fields=maps:merge(message_e:get(MsgId), #{states => message_e:get_states(MsgId), latest =>
            L#entity{fields=message_state_e:get(StateId)}})}, [id, states, channel, sender, latest]),
    none;


%% sends a direct message
handle_entity(M=#entity{type=message,       fields=#{channel:=Channel, latest:=
              L=#entity{type=message_state, fields=#{encrypted:=Encrypted}}}}, Seq, Ref) ->
    % check permissions
    #{group := Group} = channel_e:get(Channel),
    % DMs should be sent encrypted
    {_, true} = {{Ref, status_packet:make(invalid_request, "\"encrypted\" field in group message", Seq)}, Group =:= 0},
    yamka_auth:assert_permission(send_direct_messages, {Ref, Seq}),

    % create entities
    {MsgId, _} = message_e:create(Channel, get(id)),
    StateId = message_state_e:create(MsgId, Encrypted),
    % broadcast the message
    client:icpc_broadcast_to_aware(chan_awareness, Channel,
        M#entity{fields=maps:merge(message_e:get(MsgId), #{states => message_e:get_states(MsgId), latest =>
            L#entity{fields=message_state_e:get(StateId)}})}, [id, states, channel, sender, latest]),
    none;


%% edits a group message
handle_entity(M=#entity{type=message,       fields=#{id:=Id, latest:=
              L=#entity{type=message_state, fields=#{sections:=Sections}}}}, Seq, Ref) ->
    #{channel := Channel} = Existing = message_e:get(Id),
    #{group := Group} = channel_e:get(Channel),
    {_, true} = {{Ref, status_packet:make(invalid_request, "\"sections\" field in direct message", Seq)}, Group > 0},
    yamka_auth:assert_permission(send_group_messages, {Ref, Seq}),
    SelfSent = maps:get(sender, Existing) =:= get(id),
    {_, true} = {{Ref, status_packet:make(permission_denied, "This message was sent by another user", Seq)},
        SelfSent},

    StateId = message_state_e:create(Id, message_state_e:filter_sections(Sections)),
    client:icpc_broadcast_to_aware(chan_awareness, maps:get(channel, Existing),
        M#entity{fields=maps:merge(message_e:get(Id), #{states => message_e:get_states(Id), latest =>
            L#entity{fields=message_state_e:get(StateId)}})}, [id, states, channel, sender, latest]),
    none;


%% edits a direct message
handle_entity(M=#entity{type=message,       fields=#{id:=Id, latest:=
              L=#entity{type=message_state, fields=#{encrypted:=Encrypted}}}}, Seq, Ref) ->
    #{channel := Channel} = Existing = message_e:get(Id),
    #{group := Group} = channel_e:get(Channel),
    {_, true} = {{Ref, status_packet:make(invalid_request, "\"encrypted\" field in group message", Seq)}, Group =:= 0},
    yamka_auth:assert_permission(send_direct_messages, {Ref, Seq}),
    SelfSent = maps:get(sender, Existing) =:= get(id),
    {_, true} = {{Ref, status_packet:make(permission_denied, "This message was sent by another user", Seq)},
        SelfSent},

    StateId = message_state_e:create(Id, Encrypted),
    client:icpc_broadcast_to_aware(chan_awareness, maps:get(channel, Existing),
        M#entity{fields=maps:merge(message_e:get(Id), #{states => message_e:get_states(Id), latest =>
            L#entity{fields=message_state_e:get(StateId)}})}, [id, states, channel, sender, latest]),
    none;


%% deletes a message
handle_entity(M=#entity{type=message, fields=#{id:=Id, sender:=0}}, Seq, Ref) ->
    #{channel := Channel, sender := Sender} = message_e:get(Id),
    #{group := Group, lcid := Lcid} = channel_e:get(Channel),
    {_, true} = {{Ref, status_packet:make(permission_denied, "This message was sent by another user", Seq)},
        Sender =:= get(id)},
    #{group := Group} = channel_e:get(Channel),
    yamka_auth:assert_permission(if
        Group =/= 0 ->
            group_e:assert_permission(Group, delete_others_messages, {Ref, Seq}),
            delete_group_messages;
        true -> delete_direct_messages
    end, {Ref, Seq}),
    % decrease the LCID
    channel_e:update(Channel, #{lcid => Lcid - 1}),
    message_e:delete(Id),
    client:icpc_broadcast_to_aware(chan_awareness, Channel,
        #entity{type=message, fields=#{id => Id, channel => Channel, sender => 0}},
            [id, channel, sender]),
    none;


%% creates a group
handle_entity(#entity{type=group, fields=#{id:=0, name:=Name}}, Seq, Ref) ->
    yamka_auth:assert_permission(create_groups, {Ref, Seq}),
    #{name := Username} = user_e:get(get(id)),
    {Id, Everyone} = group_e:create(Name, get(id)),
    role_e:add(Everyone, get(id)),
    group_e:cache_user_name(Id, get(id), Username),
    client:icpc_broadcast_entity(get(id),
        #entity{type=user, fields=user_e:get(get(id))}, [groups]),
    none;


%% deletes a group
handle_entity(#entity{type=group, fields=#{id:=Id, owner:=0}}, Seq, Ref) ->
    yamka_auth:assert_permission(delete_groups, {Ref, Seq}),
    #{owner := Owner,
      channels := Channels,
      roles := Roles,
      invites := Invites,
      everyone_role := Everyone} = group_e:get(Id),
    {_, Owner} = {{Ref, status_packet:make(permission_denied, "Only the owner can do this", Seq)}, get(id)},

    lists:foreach(fun(R) -> group_e:remove_invite(Id, R) end, Invites),
    group_e:delete(Id),
    lists:foreach(fun channel_e:delete/1, Channels),
    lists:foreach(fun role_e:nuke/1, lists:delete(Everyone, Roles)),
    role_e:nuke(Everyone, true),
    client:icpc_broadcast_to_aware(group_awareness,
        #entity{type=group, fields=#{id => Id, owner => 0}}, [id, owner]),
    none;


%% manages invites
handle_entity(#entity{type=group, fields=#{id:=Id, invites:=Invites}}, Seq, Ref) ->
    yamka_auth:assert_permission(edit_groups, {Ref, Seq}),
    group_e:assert_permission(Id, edit_invites, {Ref, Seq}),

    #{invites := ExInvites} = group_e:get(Id),
    {Added, Removed} = utils:list_diff(Invites, ExInvites),
    lists:foreach(fun(_) -> group_e:add_invite(Id) end, lists:seq(1, length(Added))),
    lists:foreach(fun(I) -> group_e:remove_invite(Id, I) end, Removed),
    #{invites := NewInvites} = group_e:get(Id),
    entities_packet:make([#entity{type=group, fields=#{id => Id, invites => NewInvites}}], Seq);


%% manages emoji
handle_entity(#entity{type=group, fields=#{id:=Id, emoji:=Emoji}}, Seq, Ref) ->
    yamka_auth:assert_permission(edit_groups, {Ref, Seq}),
    group_e:assert_permission(Id, edit_emoji, {Ref, Seq}),

    #{emoji := ExEmoji} = group_e:get(Id),
    {Added, Removed} = utils:list_diff(Emoji, ExEmoji),
    lists:foreach(fun(I) ->
        #{emoji_name := EmojiName} = file_e:get(I),
        file_e:update(I, #{emoji_group => Id})
    end, Added),
    % don't delete the file, just deasscoiate it
    lists:foreach(fun(I) -> file_e:update(I, #{emoji_group => null}) end, Removed),
    #{emoji := NewEmoji} = group_e:get(Id),
    entities_packet:make([#entity{type=group, fields=#{id => Id, emoji => NewEmoji}}], Seq);


%% creates a poll
handle_entity(#entity{type=poll, fields=#{id:=0, options:=Options}}, Seq, Ref) ->
    yamka_auth:assert_permission(create_polls, {Ref, Seq}),

    {_, false} = {{Ref, status_packet:make(poll_error, "Zero or too many options", Seq)},
        (length(Options) > 10) or (length(Options) == 0)},
    Id = poll_e:create(Options),
    ets:insert(poll_awareness, {Id, {get(id), self()}}),
    entities_packet:make([#entity{type=poll, fields=#{
        id => Id, options => Options, total_votes => 0,
        option_votes => [0 || _ <- Options]}}], Seq);


%% votes in a poll
handle_entity(#entity{type=poll, fields=#{id:=Id, self_vote:=Option}}, Seq, Ref) ->
    yamka_auth:assert_permission(vote_in_polls, {Ref, Seq}),

    {_, {error, novote}} = {{Ref, status_packet:make(poll_error, "Already voted", Seq)},
        poll_e:get_vote(Id, get(id))},
    {_, ok} = {status_packet:make(poll_error, "Invalid option", Seq), poll_e:vote(Id, get(id), Option)},
    client:icpc_broadcast_to_aware(poll_awareness,
        #entity{type=poll, fields=poll_e:get(Id)}, [id, total_votes, option_votes]),
    none;


%% publishes keys
handle_entity(#entity{type=pkey, fields=#{type:=id_sign, key:=KeyBin}}, _Seq, _Ref) ->
    % see if there's an identity key already
    case pkey_e:get_by_user(get(id), id_sign) of
        [] -> ok;
        [Existing] ->
            % TODO: SCREAM OUT LOUD TO EVERY USER THIS ONE SHARES A DM CHANNEL WITH
            pkey_e:delete(Existing)
    end,
    pkey_e:create(get(id), id_sign, KeyBin, null),
    none;
handle_entity(#entity{type=pkey, fields=#{type:=Type, key:=KeyBin, signature:=SignBin}}, _Seq, _Ref)
            when Type =:= identity; Type =:= prekey ->
    % TODO: it would be a good idea to check the signature
    % see if there's a key already
    case pkey_e:get_by_user(get(id), Type) of
        [] -> ok;
        [Existing] -> pkey_e:delete(Existing)
    end,
    pkey_e:create(get(id), Type, KeyBin, SignBin),
    none;
handle_entity(#entity{type=pkey, fields=#{type:=otprekey, key:=KeyBin, signature:=SignBin}}, Seq, Ref) ->
    % TODO: again, it would be a good idea to check the signature
    Count = pkey_e:count(get(id), otprekey),
    {_, false} = {{Ref, status_packet:make(key_error, "Reached the limit of 256 one-time prekeys", Seq)}, Count >= 256},
    pkey_e:create(get(id), otprekey, KeyBin, SignBin),
    none;


%% handle illegal requests
handle_entity(#entity{}, Seq, _Ref) ->
    status_packet:make(invalid_request, "Illegal EntityPut request (check type and id)", Seq).





%% gets a user
handle_get_request(#entity_get_rq{type=user, id=Id, pagination=none, context=none, key=none}, _Ref) ->
    ets:insert(user_awareness, {Id, {get(id), self()}}),

    true = yamka_auth:has_permission(see_profile),
    Self = user_e:get(get(id)),
    IsSelf = Id == get(id),
    Online = user_e:online(Id),
    Unfiltered = user_e:get(Id),

    OtpHashes = if
        IsSelf ->
            #{otp_hashes => [pkey_e:fingerprint(Id)
                || Id <- pkey_e:get_by_user(get(id), otprekey)]};
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
    #entity{type=user, fields=maps:merge(FilteredFields, Agents)};


%% gets a user in context of a group
handle_get_request(#entity_get_rq{type=user, id=Id, pagination=none, context=
        #entity_context{type=group, id=_Group}, key=none}, _Ref) ->
    % TODO
    #entity{type=user, fields=#{id => Id, color => 0}};


%% gets user's keys
handle_get_request(#entity_get_rq{type=user, id=Id, pagination=none, context=none, key=KeyType}, {Ref, Seq}) ->
    Keys = pkey_e:get_by_user(Id, KeyType),
    {_, true} = {{Ref, status_packet:make(key_error, "No key(s) of type " ++ atom_to_list(KeyType), Seq)},
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
                    {_, true} = {{Ref, status_packet:make(key_error, "One-time prekey rate limiting", Seq)},
                        pkey_e:check_rate_limit(Id, get(id))},
                    pkey_e:limit_rate(Id, get(id)),
                    pkey_e:delete(Key);
                true -> ok
            end,
            #entity{type=user, fields=#{id => Id, KeyType =>
                #entity{type=pkey, fields=KeyObj}}}
    end;


%% gets a file
handle_get_request(#entity_get_rq{type=file, id=Id, pagination=none, context=none, key=none}, _Ref) ->
    ets:insert(file_awareness, {Id, {get(id), self()}}),
    % there are no permission restrictions on file accesses
    #entity{type=file, fields=file_e:get(Id)};


%% gets a channel
handle_get_request(#entity_get_rq{type=channel, id=Id, pagination=none, context=none, key=none}, _Ref) ->
    ets:insert(chan_awareness, {Id, {get(id), self()}}),

    Unfiltered = channel_e:get(Id),
    {UnreadLcid, UnreadId} = channel_e:get_unread(Id, get(id)),
    Mentions = channel_e:get_mentions(Id, get(id)),
    Filtered = maps:filter(fun(K, _) -> K =/= perms end, Unfiltered),
    UnreadCnt = maps:get(lcid, Unfiltered) - UnreadLcid,
    AddMap = case UnreadCnt of
            N when N =< 0 -> #{unread => 0};
            _ -> #{unread => UnreadCnt, first_unread => UnreadId}
        end,
    #entity{type=channel, fields=maps:merge(
        maps:merge(Filtered, AddMap),
        #{typing   => channel_e:get_typing(Id),
          mentions => Mentions})};


%% gets channel messages
handle_get_request(#entity_get_rq{type=channel, id=Id, pagination=#entity_pagination{
        field=4, dir=Dir, from=From, cnt=Cnt}, context=none, key=none}, Ref) ->
    #{group := Group} = channel_e:get(Id),
    yamka_auth:assert_permission(if
        Group =/= 0 ->
            group_e:assert_permission(Group, read_messages, Ref),
            read_group_message_history;
        true -> read_direct_message_history
    end, Ref),
    #entity{type=channel, fields=#{id => Id, messages => channel_e:get_messages(Id, From, Cnt, Dir)}};


%% gets a message by id
handle_get_request(#entity_get_rq{type=message, id=Id, pagination=none, context=none, key=none}, _Ref) ->
    Filtered = maps:filter(fun(K, _) -> K /= lcid end, message_e:get(Id)),
    StateMap = #{states => message_e:get_states(Id), latest => #entity{type=message_state, fields=
        message_state_e:get(message_e:get_latest_state(Id))}},
    #entity{type=message, fields=maps:merge(Filtered, StateMap)};


%% gets a message state by id
handle_get_request(#entity_get_rq{type=message_state, id=Id, pagination=none, context=none, key=none}, _Ref) ->
    #entity{type=message_state, fields=message_state_e:get(Id)};


%% gets a group by id
handle_get_request(#entity_get_rq{type=group, id=Id, pagination=none, context=none, key=none}, _Ref) ->
    ets:insert(group_awareness, {Id, {get(id), self()}}),
    #entity{type=group, fields=group_e:get(Id)};


%% gets a role by id
handle_get_request(#entity_get_rq{type=role, id=Id, pagination=none, context=none, key=none}, _Ref) ->
    #entity{type=role, fields=role_e:get(Id)};


%% gets role members
handle_get_request(#entity_get_rq{type=role, id=Id, pagination=#entity_pagination{
        field=6, dir=Dir, from=From, cnt=Cnt}, context=none, key=none}, Ref) ->
    #{group := Group} = role_e:get(Id),
    group_e:assert_permission(Group, see_members, Ref),
    #entity{type=role, fields=#{id => Id, members => role_e:get_members(Id, From, Cnt, Dir)}};


%% gets a poll by id
handle_get_request(#entity_get_rq{type=poll, id=Id, pagination=none, context=none, key=none}, _Ref) ->
    ets:insert(poll_awareness, {Id, {get(id), self()}}),
    Main = poll_e:get(Id),
    Fields = case poll_e:get_vote(Id, get(id)) of
        {error, novote} -> Main;
        {ok, Option}    -> maps:merge(Main, #{self_vote => Option})
    end,
    #entity{type=poll, fields=Fields};


%% gets an agent by id
handle_get_request(#entity_get_rq{type=agent, id=Id, pagination=none, context=none, key=none}, _Ref) ->
    Online = #{online => agent_e:online(Id)},
    #entity{type=agent, fields=maps:merge(agent_e:get(Id), Online)};