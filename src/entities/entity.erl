%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(entity).
-author("Yamka").
-license("MPL-2.0").

-include("../packets/packet.hrl").
-include("entity.hrl").

-export([handle_get_request/2, handle_entity/3]).
-export([encode_field/2, encode/2, len_decode/2]).
-export([construct_kv_str/1, filter/2]).

structure(Proto, EntityType) ->
    #{Proto := #{EntityType := Structure}} = ?ENTITY_STRUCTURE,
    Structure.

%% updates a user
handle_entity(#entity{type=user, fields=#{id:=Id} = F}, Seq, ScopeRef) ->
    % a user can only change info about themselves
    yamka_auth:assert_permission(edit_profile, {ScopeRef, Seq}),
    {_, Id} = {{ScopeRef, status_packet:make(invalid_id, "You can only change info about yourself", Seq)}, get(id)},
    % only allow allowed fields (wow thx captain obvious)
    BroadcastFields = [name, status, status_text, ava_file],
    AllowedFields = maps:filter(fun(K, _) -> lists:member(K, [email|BroadcastFields]) end, F),
    case maps:size(AllowedFields) of
        0 -> ok;
        _ ->
            % change the DB record
            user_e:update(Id, AllowedFields),
            % broadcast the changes
            Entity = #entity{type=user, fields=maps:merge(AllowedFields, #{id=>Id})},
            normal_client:icpc_broadcast_to_aware(Entity, BroadcastFields)
    end,
    % if the email was changed, un-confirm it and send a confirmation request to the user
    EmailChanged = maps:is_key(email, F),
    if
        EmailChanged ->
            Email = maps:get(email, F),
            user_e:update(Id, #{email => Email, email_confirmed => false}),
            user_e:start_email_confirmation(Id, Email),
            NewEntity = #entity{type=user, fields=#{
                id => Id,
                email => Email,
                email_confirmed => false}},
            normal_client:icpc_broadcast_entity(Id, NewEntity);
        true -> ok
    end,
    % if the 2FA setting has changed, update auth settings
    MfaChanged = maps:is_key(mfa_enabled, F),
    if
        MfaChanged ->
            MfaEnabled = maps:get(mfa_enabled, F),
            normal_client:icpc_broadcast_entity(Id, #entity{type=user,
                fields=#{id => Id, mfa_enabled => MfaEnabled}}, [mfa_enabled]),
            if
                MfaEnabled ->
                    Secret = yamka_auth:totp_secret(),
                    user_e:update(Id, #{mfa_secret => Secret}),
                    mfa_secret_packet:make(Secret, Seq);
                true ->
                    user_e:update(Id, #{mfa_secret => null}),
                    none
            end;
        true ->
            none
    end;

%% puts a file
handle_entity(#entity{type=file, fields=#{name:=Name, length:=Length}}, Seq, ScopeRef) ->
    {_, none} = {{ScopeRef, status_packet:make(one_upload_only,
            "Only one concurrent upload is allowed", Seq)}, get(file_recv_pid)},
    put(file_recv_pid, file_storage:recv_file(Seq,
        {get(socket), get(protocol), self(), get(cassandra)},
        {Length, Name})),
    none;

%% (re-)sets the typing status
handle_entity(#entity{type=channel, fields=#{id:=Id, typing:=[0]}}, _Seq, _ScopeRef) ->
    channel:set_typing(Id, get(id)),
    normal_client:icpc_broadcast_to_aware(chan_awareness,
        #entity{type=channel, fields=#{id=>Id, typing=>channel:get_typing(Id)}}, [id, typing]),
    none;
handle_entity(#entity{type=channel, fields=#{id:=Id, typing:=[]}}, _Seq, _ScopeRef) ->
    channel:reset_typing(Id, get(id)), 
    normal_client:icpc_broadcast_to_aware(chan_awareness,
        #entity{type=channel, fields=#{id=>Id, typing=>channel:get_typing(Id)}}, [id, typing]),
    none;

%% sets the unread message
handle_entity(#entity{type=channel, fields=#{id:=Id, first_unread:=FirstUnread}}, _Seq, _ScopeRef) ->
    % get the message and its LCID
    #{lcid := Lcid} = message:get(FirstUnread),
    channel:set_unread(Id, get(id), {Lcid, FirstUnread}),
    none;
%% marks the channel as read
handle_entity(#entity{type=channel, fields=#{id:=Id, unread:=0}}, _Seq, _ScopeRef) ->
    % get the last message and its LCID
    case channel:get_messages(Id, 9223372036854775807, 1, down) of
        [] -> ok;
        [LastMsg] ->
            #{lcid := Lcid} = message:get(LastMsg),
            channel:set_unread(Id, get(id), {Lcid, LastMsg})
    end,
    none;

%% creates a channel
handle_entity(#entity{type=channel, fields=#{id:=0, group:=Group, name:=Name}}, Seq, ScopeRef) ->
    yamka_auth:assert_permission(edit_groups, {ScopeRef, Seq}),
    #{owner := Owner} = group_e:get(Group),
    {_, Owner} = {{ScopeRef, status_packet:make(permission_denied, "No administrative permission", Seq)}, get(id)},
    channel:create(Name, Group, [], false),
    normal_client:icpc_broadcast_to_aware(group_awareness, #entity{
        type=group, fields=group_e:get(Group)}, [id, channels]),
    none;

%% deletes a channel
handle_entity(#entity{type=channel, fields=#{id:=Id, group:=0}}, Seq, ScopeRef) ->
    yamka_auth:assert_permission(edit_groups, {ScopeRef, Seq}),
    % get existing channel's group
    #{group := Group} = channel:get(Id),
    % check group ownership
    #{owner := Owner} = group_e:get(Group),
    {_, Owner} = {{ScopeRef, status_packet:make(permission_denied, "No administrative permission", Seq)}, get(id)},
    % modify channel
    channel:delete(Id),
    % broadcast updates
    normal_client:icpc_broadcast_to_aware(group_awareness, #entity{
        type=group, fields=group_e:get(Group)}, [id, channels]),
    none;

%% modifies a channel
handle_entity(#entity{type=channel, fields=Fields=#{id:=Id}}, Seq, ScopeRef) ->
    yamka_auth:assert_permission(edit_groups, {ScopeRef, Seq}),
    % get existing channel's group
    #{group := Group} = channel:get(Id),
    % check group ownership
    #{owner := Owner} = group_e:get(Group),
    {_, Owner} = {{ScopeRef, status_packet:make(permission_denied, "No administrative permission", Seq)}, get(id)},
    % modify channel
    channel:update(Id, maps:filter(fun(K, _) -> K =/= id end, Fields)),
    % broadcast updates
    normal_client:icpc_broadcast_to_aware(chan_awareness,
        #entity{type=channel, fields=Fields}, maps:keys(Fields)),
    none;

%% sends a message
handle_entity(M=#entity{type=message,       fields=#{id:=0, channel:=Channel, latest:=
              L=#entity{type=message_state, fields=#{id:=0, sections:=Sections}}}}, Seq, ScopeRef) ->
    % cheeck token permissions
    #{group := Group} = channel:get(Channel),
    yamka_auth:assert_permission(if
        Group =/= 0 -> send_group_messages;
        true -> send_direct_messages
    end, {ScopeRef, Seq}),
    % parse mentions
    Filtered = message_state:filter_sections(Sections),
    Mentions = message_state:parse_mentions(Filtered),
    % create entities
    {MsgId, MsgLcid} = message:create(Channel, get(id)),
    StateId = message_state:create(MsgId, Filtered),
    channel:reg_msg(Channel, MsgId),
    % register mentions
    [channel:set_mention(Channel, User, {MsgLcid, MsgId}) || User <- Mentions],
    % broadcast the message
    normal_client:icpc_broadcast_to_aware(chan_awareness, Channel,
        M#entity{fields=maps:merge(message:get(MsgId), #{states => message:get_states(MsgId), latest =>
            L#entity{fields=message_state:get(StateId)}})}, [id, states, channel, sender, latest]),
    none;

%% edits a message
handle_entity(M=#entity{type=message,       fields=#{id:=Id, latest:=
              L=#entity{type=message_state, fields=#{id:=0, sections:=Sections}}}}, Seq, ScopeRef) ->
    Existing = message:get(Id),
    {_, true} = {{ScopeRef, status_packet:make(permission_denied, "This message was sent by another user", Seq)},
        maps:get(sender, Existing) =:= get(id)},
    StateId = message_state:create(Id, message_state:filter_sections(Sections)),
    % broadcast the message
    normal_client:icpc_broadcast_to_aware(chan_awareness, maps:get(channel, Existing),
        M#entity{fields=maps:merge(message:get(Id), #{states => message:get_states(Id), latest =>
            L#entity{fields=message_state:get(StateId)}})}, [id, states, channel, sender, latest]),
    none;

%% deletes a message
handle_entity(M=#entity{type=message, fields=#{id:=Id, sender:=0}}, Seq, ScopeRef) ->
    #{channel := Channel, sender := Sender} = message:get(Id),
    {_, true} = {{ScopeRef, status_packet:make(permission_denied, "This message was sent by another user", Seq)},
        Sender =:= get(id)},
    % cheeck token permissions
    #{group := Group} = channel:get(Channel),
    yamka_auth:assert_permission(if
        Group =/= 0 -> delete_group_messages;
        true -> delete_direct_messages
    end, {ScopeRef, Seq}),
    % delete message
    channel:unreg_msg(Channel, Id),
    message:delete(Id),
    % broadcast the deletion notification
    normal_client:icpc_broadcast_to_aware(chan_awareness, Channel,
        M#entity{fields=#{id => Id, channel => Channel, sender => 0}},
            [id, channel, sender]),
    none;

%% creates a group
handle_entity(#entity{type=group, fields=#{id:=0, name:=Name}}, Seq, ScopeRef) ->
    yamka_auth:assert_permission(create_groups, {ScopeRef, Seq}),
    #{name := Username} = user_e:get(get(id)),
    {Id, Everyone} = group_e:create(Name, get(id)),
    role:add(Everyone, get(id)),
    group_e:cache_user_name(Id, get(id), Username),
    user_e:manage_contact(get(id), add, {group, Id}),
    normal_client:icpc_broadcast_entity(get(id),
        #entity{type=user, fields=user_e:get(get(id))}, [groups]),
    none;

%% deletes a group
handle_entity(#entity{type=group, fields=#{id:=Id, owner:=0}}, Seq, ScopeRef) ->
    yamka_auth:assert_permission(delete_groups, {ScopeRef, Seq}),
    #{owner := Owner,
      channels := Channels,
      roles := Roles,
      invites := Invites,
      everyone_role := Everyone} = group_e:get(Id),
    {_, Owner} = {{ScopeRef, status_packet:make(permission_denied, "No administrative permission", Seq)}, get(id)},
    % nuke everything!
    lists:foreach(fun(R) -> group_e:remove_invite(Id, R) end, Invites),
    group_e:delete(Id),
    lists:foreach(fun channel:delete/1, Channels),
    lists:foreach(fun role:nuke/1, lists:delete(Everyone, Roles)),
    role:nuke(Everyone, true),
    normal_client:icpc_broadcast_to_aware(group_awareness,
        #entity{type=group, fields=#{id => Id, owner => 0}}, [id, owner]),
    none;

%% manages invites
handle_entity(#entity{type=group, fields=#{id:=Id, invites:=Invites}}, Seq, ScopeRef) ->
    yamka_auth:assert_permission(edit_groups, {ScopeRef, Seq}),
    #{owner := Owner, invites := ExInvites} = group_e:get(Id),
    {_, Owner} = {{ScopeRef, status_packet:make(permission_denied, "No administrative permission", Seq)}, get(id)},
    % calculate the list difference
    {Added, Removed} = utils:list_diff(Invites, ExInvites),
    lists:foreach(fun(_) -> group_e:add_invite(Id) end, lists:seq(1, length(Added))),
    lists:foreach(fun(I) -> group_e:remove_invite(Id, I) end, Removed),
    #{invites := NewInvites} = group_e:get(Id),
    #packet{type=entities, fields=#{entities => [#entity{type=group, fields=#{id => Id, invites => NewInvites}}]}, reply=Seq}.
    


%% gets a user
handle_get_request(#entity_get_rq{type=user, id=Id, pagination=none, context=none}, _Ref) ->
    ets:insert(user_awareness, {Id, {get(id), self()}}),

    true = yamka_auth:has_permission(see_profile),
    Self = user_e:get(get(id)),
    IsSelf = Id == get(id),
    Online = user_e:online(Id),
    Unfiltered = user_e:get(Id),

    Dm = case channel:get_dm([get(id), Id]) of
        nodm -> #{};
        DmId -> #{dm_channel => DmId}
    end,

    Mfa = maps:merge(Dm, #{mfa_enabled => maps:get(mfa_secret, Unfiltered) =/= null}),
    
    FilteredFields = maps:filter(fun(K, _) ->
        case K of
            id              -> true;
            email           -> IsSelf;
            email_confirmed -> IsSelf;
            mfa_enabled     -> IsSelf;
            name            -> true;
            tag             -> true;
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
    #entity{type=user, fields=maps:merge(FilteredFields, Mfa)};

%% gets a user in context of a group
handle_get_request(#entity_get_rq{type=user, id=Id, pagination=none, context=
        #entity_context{type=group, id=_Group}}, _Ref) ->
    % TODO
    #entity{type=user, fields=#{id => Id, color => 0}};

%% gets a file
handle_get_request(#entity_get_rq{type=file, id=Id, pagination=none, context=none}, _Ref) ->
    % there are no permission restrictions on file accesses
    #entity{type=file, fields=file_e:get(Id)};

%% gets a channel
handle_get_request(#entity_get_rq{type=channel, id=Id, pagination=none, context=none}, _Ref) ->
    ets:insert(chan_awareness, {Id, {get(id), self()}}),

    Unfiltered = channel:get(Id),
    {UnreadLcid, UnreadId} = channel:get_unread(Id, get(id)),
    {MentionLcid, MentionId} = channel:get_mention(Id, get(id)),
    Filtered = maps:filter(fun(K, _) ->
            case K of
                lcid  -> false;
                perms -> false;
                _     -> true
            end
        end, Unfiltered),
    UnreadCnt = maps:get(lcid, Unfiltered) - UnreadLcid,
    AddMap = case UnreadCnt of
            0 -> #{unread => 0};
            _ -> #{unread => UnreadCnt, first_unread => UnreadId}
        end,
    MentionCnt = maps:get(lcid, Unfiltered) - MentionLcid,
    AddMap2 = case MentionCnt of
            0 -> #{mentions => 0};
            _ -> #{mentions => MentionCnt, first_mention => MentionId}
        end,
    #entity{type=channel, fields=maps:merge(
        maps:merge(Filtered, maps:merge(AddMap, AddMap2)),
        #{typing => channel:get_typing(Id)})};

%% gets channel messages
handle_get_request(#entity_get_rq{type=channel, id=Id, pagination=#entity_pagination{
        field=4, dir=Dir, from=From, cnt=Cnt}, context=none}, Ref) ->
    #{group := Group} = channel:get(Id),
    yamka_auth:assert_permission(if
        Group =/= 0 -> read_group_message_history;
        true -> read_direct_message_history
    end, Ref),
    #entity{type=channel, fields=#{id => Id, messages => channel:get_messages(Id, From, Cnt, Dir)}};

%% gets a message by id
handle_get_request(#entity_get_rq{type=message, id=Id}, _Ref) ->
    Filtered = maps:filter(fun(K, _) -> K /= lcid end, message:get(Id)),
    StateMap = #{states => message:get_states(Id), latest => #entity{type=message_state, fields=
        message_state:get(message:get_latest_state(Id))}},
    #entity{type=message, fields=maps:merge(Filtered, StateMap)};

%% gets a message state by id
handle_get_request(#entity_get_rq{type=message_state, id=Id, pagination=none, context=none}, _Ref) ->
    #entity{type=message_state, fields=message_state:get(Id)};

%% gets a group by id
handle_get_request(#entity_get_rq{type=group, id=Id, pagination=none, context=none}, _Ref) ->
    ets:insert(group_awareness, {Id, {get(id), self()}}),
    #entity{type=group, fields=group_e:get(Id)};

%% gets a role by id
handle_get_request(#entity_get_rq{type=role, id=Id, pagination=none, context=none}, _Ref) ->
    #entity{type=role, fields=role:get(Id)};

%% gets role members
handle_get_request(#entity_get_rq{type=role, id=Id, pagination=#entity_pagination{
        field=6, dir=Dir, from=From, cnt=Cnt}, context=none}, _Ref) ->
    #entity{type=role, fields=#{id => Id, members => role:get_members(Id, From, Cnt, Dir)}}.

%% encodes entities
encode_field(_Proto, number,   V, {Size})       -> datatypes:enc_num(V, Size);
encode_field(_Proto, string,   V, {})           -> datatypes:enc_str(V);
encode_field(_Proto, atom,     V, {Size, Map})  -> datatypes:enc_num(maps:get(V, utils:swap_map(Map)), Size);
encode_field(_Proto, bool,     V, {})           -> datatypes:enc_bool(V);
encode_field(_Proto, num_list, V, {Size})       -> datatypes:enc_num_list(V, Size);
encode_field(_Proto, str_list, V, {})           -> datatypes:enc_list(V, fun datatypes:enc_str/1, 2);
encode_field(_Proto, list,     V, {LS, EF, _})  -> datatypes:enc_list(V, EF, LS);
encode_field( Proto, entity,   V, {})           -> entity:encode(V, Proto).
encode_field({{Id, Type, Args}, Value}, Proto) ->
    Repr = encode_field(Proto, Type, Value, Args),
    IdBin = datatypes:enc_num(Id, 1),
    <<IdBin/binary, Repr/binary>>.

encode(#entity{type=Type, fields=Fields}, Proto) ->
    TypeBin        = datatypes:enc_num(maps:get(Type, ?REVERSE_ENTITY_MAP), 1),
    Structure      = structure(Proto, Type),

    FieldsToEncode = utils:intersect_lists([maps:keys(Fields), maps:keys(Structure)]),
    FieldValues    = [{maps:get(K, Structure), maps:get(K, Fields)} || K <- FieldsToEncode],

    BinaryRepr = datatypes:enc_list(FieldValues,
        fun(F) -> encode_field(F, Proto) end, 1),
    <<TypeBin/binary, BinaryRepr/binary>>.

%% filters entity fields
filter(#entity{fields=Fields}=E, Allowed) ->
    E#entity{fields=maps:from_list(
        lists:filter(fun({K, _}) -> lists:member(K, [id|Allowed]) end,
            maps:to_list(Fields))
    )}.

%% finds a field descriptor by its ID in a reversed entity structure map
find_desc(RevS, Id) ->
    Desc = lists:nth(1, [D || {I, _, _} = D <- utils:map_keys(RevS), I == Id]),
    Name = maps:get(Desc, RevS),
    {Name, Desc}.

%% decodes entities
len_decode_field(_Proto, number,   V, {Size})       -> {datatypes:dec_num(V, Size), Size};
len_decode_field(_Proto, string,   V, {})           -> {datatypes:dec_str(V), datatypes:len_str(V)};
len_decode_field(_Proto, atom,     V, {Size, Map})  -> {maps:get(datatypes:dec_num(V, Size), Map), Size};
len_decode_field(_Proto, bool,     V, {})           -> {datatypes:dec_bool(V), 1};
len_decode_field(_Proto, num_list, V, {Size})       -> R=datatypes:dec_num_list(V, Size), {R, 2+(length(R)*Size)};
len_decode_field(_Proto, str_list, V, {})           -> datatypes:len_dec_list(V, fun datatypes:len_dec_str/1, 2);
len_decode_field(_Proto, list,     V, {LS, _, LDF}) -> datatypes:len_dec_list(V, LDF, LS);
len_decode_field( Proto, entity,   V, {})           -> entity:len_decode(V, Proto).
len_decode_field(RevStructure, Bin, Proto) ->
    <<Id:8/unsigned-integer, Repr/binary>> = Bin,
    % find the descriptor
    {Name, {Id, Type, Args}} = find_desc(RevStructure, Id),
    {FieldVal, Len} = len_decode_field(Proto, Type, Repr, Args),
    {{Name, FieldVal}, Len + 1}. % +1 because we have a one byte field ID

len_decode(Bin, Proto) ->
    <<TypeNum:8/unsigned-integer, FieldsBin/binary>> = Bin,
    Type = maps:get(TypeNum, ?ENTITY_TYPE_MAP),
    RevStructure = utils:swap_map(structure(Proto, Type)),
    {FieldProplist, Len} = datatypes:len_dec_list(FieldsBin,
        fun(B) -> len_decode_field(RevStructure, B, Proto) end, 1),
    Fields = maps:from_list(FieldProplist),

    {#entity{type=Type, fields=Fields}, Len}.

%% constructs a "key1=?,key2=? ..." string and a list of cqerl bindings from #{key1=>val1, key2=>val2, ...}
construct_kv_str(Map) when is_map(Map) -> construct_kv_str(maps:to_list(Map));
construct_kv_str([]) -> {"", []};
construct_kv_str([{K,V}|T]) ->
    {RestStr, RestBind} = construct_kv_str(T),
    {atom_to_list(K) ++ "=?" ++ case RestStr of""->"";_->","end ++ RestStr, [{K,V}|RestBind]}.