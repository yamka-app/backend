-module(entity).
-author("Order").
-license("MPL-2.0").

-include("../packets/packet.hrl").
-include("entity.hrl").

-export([handle_get_request/1, handle_entity/3]).
-export([encode_field/1, encode/1, len_decode/1]).
-export([construct_kv_str/1, filter/2]).

%% updates a user
handle_entity(#entity{type=user, fields=#{id:=Id} = F}, Seq, ScopeRef) ->
    % a user can only change info about themselves
    {_, Id} = {{ScopeRef, status_packet:make(invalid_id, "You can only change info about yourself", Seq)}, get(id)},
    % only allow allowed fields (wow thx captain obvious)
    AllowedFields = maps:filter(fun(K, _) -> lists:member(K, [
        email, name, status, status_text, ava_file
    ]) end, F),
    % change the DB record
    user:update(Id, AllowedFields),
    % broadcast the changes
    normal_client:icpc_broadcast_to_aware(#entity{type=user,
        fields=maps:merge(AllowedFields, #{id=>Id})},
        maps:keys(AllowedFields)),
    none;

%% puts a file
handle_entity(#entity{type=file, fields=#{name:=Name, length:=Length}}, Seq, ScopeRef) ->
    {_, none} = {{ScopeRef, status_packet:make(one_upload_only, "Only one concurrent upload is allowed", Seq)}, get(file_recv_pid)},
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
    #{owner := Owner} = group_e:get(Group),
    {_, Owner} = {{ScopeRef, status_packet:make(permission_denied, "No administrative permission", Seq)}, get(id)},
    channel:create(normal, Name, Group, [], false),
    normal_client:icpc_broadcast_to_aware(group_awareness, #entity{
        type=group, fields=group_e:get(Group)}, [id, channels]),
    none;

%% modifies a channel
handle_entity(#entity{type=channel, fields=Fields=#{id:=Id}}, Seq, ScopeRef) ->
    % get existing channel's group
    #{group := Group} = channel:get(Id),
    % check group ownership
    #{owner := Owner} = group_e:get(Group),
    {_, Owner} = {{ScopeRef, status_packet:make(permission_denied, "No administrative permission", Seq)}, get(id)},
    % modify channel
    logging:warn("~p", [Fields]),
    channel:update(Id, maps:filter(fun(K, _) -> K =/= id end, Fields)),
    % broadcast updates
    normal_client:icpc_broadcast_to_aware(chan_awareness,
        #entity{type=channel, fields=Fields}, maps:keys(Fields)),
    none;

%% sends a message
handle_entity(M=#entity{type=message,       fields=#{id:=0, channel:=Channel, latest:=
              L=#entity{type=message_state, fields=#{id:=0, sections:=Sections}}}}, _Seq, _ScopeRef) ->
    MsgId = message:create(Channel, get(id)),
    StateId = message_state:create(MsgId, message_state:filter_sections(Sections)),
    channel:reg_msg(Channel, MsgId),
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
    Existing = message:get(Id),
    {_, true} = {{ScopeRef, status_packet:make(permission_denied, "This message was sent by another user", Seq)},
        maps:get(sender, Existing) =:= get(id)},
    message:delete(Id),
    channel:unreg_msg(maps:get(channel, Existing), Id),
    % broadcast the deletion notification
    normal_client:icpc_broadcast_to_aware(chan_awareness, maps:get(channel, Existing),
        M#entity{fields=#{id => Id, channel => maps:get(channel, Existing), sender => 0}},
            [id, channel, sender]),
    none;

%% creates a group
handle_entity(#entity{type=group, fields=#{id:=0, name:=Name}}, _Seq, _ScopeRef) ->
    {Id, Everyone} = group_e:create(Name, get(id)),
    role:add(Everyone, get(id)),
    user:manage_contact(get(id), add, {group, Id}),
    normal_client:icpc_broadcast_entity(get(id),
        #entity{type=user, fields=user:get(get(id))}, [groups]),
    none;

%% manages invites
handle_entity(#entity{type=group, fields=#{id:=Id, invites:=Invites}}, Seq, ScopeRef) ->
    #{owner := Owner, invites := ExInvites} = group_e:get(Id),
    {_, Owner} = {{ScopeRef, status_packet:make(permission_denied, "No administrative permission", Seq)}, get(id)},
    % calculate the list difference
    {Added, Removed} = utils:list_diff(Invites, ExInvites),
    lists:foreach(fun(_) -> group_e:add_invite(Id) end, lists:seq(1, length(Added))),
    lists:foreach(fun(I) -> group_e:remove_invite(Id, I) end, Removed),
    #{invites := NewInvites} = group_e:get(Id),
    #packet{type=entities, fields=#{entities => [#entity{type=group, fields=#{id => Id, invites => NewInvites}}]}, reply=Seq}.
    


%% gets a user
handle_get_request(#entity_get_rq{type=user, id=Id, pagination=none, context=none}) ->
    ets:insert(user_awareness, {Id, {get(id), self()}}),

    Dm = case utils:safe_call(fun channel:get_dm/1, [[get(id), Id]], [{cassandra, get(cassandra)}]) of
        {error, _} -> #{};
        {ok, DmId} -> #{dm_channel => DmId}
    end,

    true = auth:has_permission(see_profile),
    IsSelf = Id == get(id), Online = user:online(Id),
    Self = user:get(get(id)),
    Unfiltered = user:get(Id),
    
    FilteredFields = maps:filter(fun(K, _) ->
        case K of
            id              -> true;
            email           -> IsSelf;
            email_confirmed -> IsSelf;
            name            -> true;
            tag             -> true;
            status          -> true;
            status_text     -> true;
            ava_file        -> true;
            friends         -> auth:has_permission(see_relationships);
            blocked         -> auth:has_permission(see_relationships) and IsSelf;
            pending_in      -> auth:has_permission(see_relationships) and IsSelf;
            pending_out     -> auth:has_permission(see_relationships) and IsSelf;
            dm_channel      -> auth:has_permission(see_direct_messages);
            groups          -> auth:has_permission(see_groups);
            badges          -> true;
            bot_owner       -> true;
            wall            -> maps:get(bot_owner, Unfiltered) == 0;
            _ -> false
        end
    end, maps:map(fun(K, V) ->
            case K of
                status -> if
                        (V /= offline) and not Online -> offline;
                        true -> V
                    end;
                groups          -> utils:intersect_lists([V, maps:get(groups,  Self)]);
                friends         -> utils:intersect_lists([V, maps:get(friends, Self)]);
                email_confirmed -> true; % for now
                _ -> V
            end
        end, Unfiltered)),
    #entity{type=user, fields=maps:merge(FilteredFields, Dm)};

%% gets a user in context of a group
handle_get_request(#entity_get_rq{type=user, id=Id, pagination=none, context=
        #entity_context{type=group, id=_Group}}) ->
    % TODO
    #entity{type=user, fields=#{id => Id, color => 0}};

%% gets a file
handle_get_request(#entity_get_rq{type=file, id=Id, pagination=none, context=none}) ->
    % there are no permission restrictions on file accesses
    #entity{type=file, fields=file_e:get(Id)};

%% gets a channel
handle_get_request(#entity_get_rq{type=channel, id=Id, pagination=none, context=none}) ->
    ets:insert(chan_awareness, {Id, {get(id), self()}}),

    Unfiltered = channel:get(Id),
    {UnreadLcid, UnreadId} = channel:get_unread(Id, get(id)),
    Filtered = maps:filter(fun(K, _) ->
            case K of
                lcid  -> false;
                perms -> false;
                _     -> true
            end
        end, Unfiltered),
    UnreadCnt = maps:get(lcid, Unfiltered) - UnreadLcid,
    AddMap = case UnreadCnt of
            0 -> #{typing => channel:get_typing(Id), unread => UnreadCnt};
            _ -> #{typing => channel:get_typing(Id), unread => UnreadCnt, first_unread => UnreadId}
        end,
    #entity{type=channel, fields=maps:merge(maps:merge(Filtered, AddMap), #{typing => []})};

%% gets channel messages
handle_get_request(#entity_get_rq{type=channel, id=Id, pagination=#entity_pagination{
        field=4, dir=Dir, from=From, cnt=Cnt}, context=none}) ->
    #entity{type=channel, fields=#{id => Id, messages => channel:get_messages(Id, From, Cnt, Dir)}};

%% gets a message by id
handle_get_request(#entity_get_rq{type=message, id=Id}) ->
    Filtered = maps:filter(fun(K, _) -> K /= lcid end, message:get(Id)),
    StateMap = #{states => message:get_states(Id), latest => #entity{type=message_state, fields=
        message_state:get(message:get_latest_state(Id))}},
    #entity{type=message, fields=maps:merge(Filtered, StateMap)};

%% gets a message state by id
handle_get_request(#entity_get_rq{type=message_state, id=Id, pagination=none, context=none}) ->
    #entity{type=message_state, fields=message_state:get(Id)};

%% gets a group by id
handle_get_request(#entity_get_rq{type=group, id=Id, pagination=none, context=none}) ->
    ets:insert(group_awareness, {Id, {get(id), self()}}),
    #entity{type=group, fields=group_e:get(Id)};

%% gets a role by id
handle_get_request(#entity_get_rq{type=role, id=Id, pagination=none, context=none}) ->
    #entity{type=role, fields=role:get(Id)};

%% gets role members
handle_get_request(#entity_get_rq{type=role, id=Id, pagination=#entity_pagination{
        field=6, dir=Dir, from=From, cnt=Cnt}, context=none}) ->
    #entity{type=role, fields=#{id => Id, members => role:get_members(Id, From, Cnt, Dir)}}.

%% encodes entities
encode_field(number,   V, {Size})       -> datatypes:enc_num(V, Size);
encode_field(string,   V, {})           -> datatypes:enc_str(V);
encode_field(atom,     V, {Size, Map})  -> datatypes:enc_num(maps:get(V, utils:swap_map(Map)), Size);
encode_field(bool,     V, {})           -> datatypes:enc_bool(V);
encode_field(num_list, V, {Size})       -> datatypes:enc_num_list(V, Size);
encode_field(str_list, V, {})           -> datatypes:enc_list(V, fun datatypes:enc_str/1, 2);
encode_field(list,     V, {LS, EF, _})  -> datatypes:enc_list(V, EF, LS);
encode_field(entity,   V, {})           -> entity:encode(V).
encode_field({{Id, Type, Args}, Value}) ->
    Repr = encode_field(Type, Value, Args),
    IdBin = datatypes:enc_num(Id, 1),
    <<IdBin/binary, Repr/binary>>.

encode(#entity{type=Type, fields=Fields}) ->
    TypeBin        = datatypes:enc_num(maps:get(Type, ?REVERSE_ENTITY_MAP), 1),
    Structure      = maps:get(Type, ?ENTITY_STRUCTURE),

    FieldsToEncode = utils:intersect_lists([maps:keys(Fields), maps:keys(Structure)]),
    FieldValues    = [{maps:get(K, Structure), maps:get(K, Fields)} || K <- FieldsToEncode],

    BinaryRepr     = datatypes:enc_list(FieldValues, fun encode_field/1, 1),
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
len_decode_field(number,   V, {Size})       -> {datatypes:dec_num(V, Size), Size};
len_decode_field(string,   V, {})           -> {datatypes:dec_str(V), datatypes:len_str(V)};
len_decode_field(atom,     V, {Size, Map})  -> {maps:get(datatypes:dec_num(V, Size), Map), Size};
len_decode_field(bool,     V, {})           -> {datatypes:dec_bool(V), 1};
len_decode_field(num_list, V, {Size})       -> R=datatypes:dec_num_list(V, Size), {R, 2+(length(R)*Size)};
len_decode_field(str_list, V, {})           -> datatypes:len_dec_list(V, fun datatypes:len_dec_str/1, 2);
len_decode_field(list,     V, {LS, _, LDF}) -> datatypes:len_dec_list(V, LDF, LS);
len_decode_field(entity,   V, {})           -> entity:len_decode(V).
len_decode_field(RevStructure, Bin) ->
    <<Id:8/unsigned-integer, Repr/binary>> = Bin,
    % find the descriptor
    {Name, {Id, Type, Args}} = find_desc(RevStructure, Id),
    {FieldVal, Len} = len_decode_field(Type, Repr, Args),
    {{Name, FieldVal}, Len + 1}. % +1 because we have a one byte field ID

len_decode(Bin) ->
    <<TypeNum:8/unsigned-integer, FieldsBin/binary>> = Bin,
    Type = maps:get(TypeNum, ?ENTITY_TYPE_MAP),
    RevStructure = utils:swap_map(maps:get(Type, ?ENTITY_STRUCTURE)),
    {FieldProplist, Len} = datatypes:len_dec_list(FieldsBin, fun(B) -> len_decode_field(RevStructure, B) end, 1),
    Fields = maps:from_list(FieldProplist),

    {#entity{type=Type, fields=Fields}, Len}.

%% constructs a "key1=?,key2=? ..." string and a list of cqerl bindings from #{key1=>val1, key2=>val2, ...}
construct_kv_str(Map) when is_map(Map) -> construct_kv_str(maps:to_list(Map));
construct_kv_str([]) -> {"", []};
construct_kv_str([{K,V}|T]) ->
    {RestStr, RestBind} = construct_kv_str(T),
    {atom_to_list(K) ++ "=?" ++ case RestStr of""->"";_->","end ++ RestStr, [{K,V}|RestBind]}.