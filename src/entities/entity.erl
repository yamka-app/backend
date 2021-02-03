-module(entity).
-author("Order").
-license("MPL-2.0").

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
    none.
    

%% gets a user
handle_get_request(#entity_get_rq{type=user, id=Id, pagination=none, context=none}) ->
    ets:insert(user_awareness, {Id, {get(id), self()}}),

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
            blocked         -> auth:has_permission(see_relationships)   and IsSelf;
            pending_in      -> auth:has_permission(see_relationships)   and IsSelf;
            pending_out     -> auth:has_permission(see_relationships)   and IsSelf;
            dm_channels     -> auth:has_permission(see_direct_messages) and IsSelf;
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
                groups  -> utils:intersect_lists([V, maps:get(groups,  Self)]);
                friends -> utils:intersect_lists([V, maps:get(friends, Self)]);
                email_confirmed -> true; % for now
                _ -> V
            end
        end, Unfiltered)),
    #entity{type=user, fields=FilteredFields};

%% gets a file
handle_get_request(#entity_get_rq{type=file, id=Id, pagination=none, context=none}) ->
    % there are no permission restrictions on file accesses
    #entity{type=file, fields=file_e:get(Id)}.

%% encodes entities
encode_field(number,   V, {Size})       -> datatypes:enc_num(V, Size);
encode_field(string,   V, {})           -> datatypes:enc_str(V);
encode_field(atom,     V, {Size, Map})  -> datatypes:enc_num(maps:get(V, utils:swap_map(Map)), Size);
encode_field(bool,     V, {})           -> datatypes:enc_bool(V);
encode_field(num_list, V, {Size})       -> datatypes:enc_num_list(V, Size);
encode_field(list,     V, {LS, EF, _})  -> datatypes:enc_list(V, EF, LS).
encode_field({{Id, Type, Args}, Value}) ->
    Repr = encode_field(Type, Value, Args),
    IdBin = datatypes:enc_num(Id, 1),
    <<IdBin/binary, Repr/binary>>.

encode(#entity{type=Type, fields=Fields}) ->
    TypeBin        = datatypes:enc_num(maps:get(Type, ?REVERSE_ENTITY_MAP), 1),
    Structure      = maps:get(Type, ?ENTITY_STRUCTURE),

    FieldsToEncode = utils:intersect_lists([utils:map_keys(Fields), utils:map_keys(Structure)]),
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
len_decode_field(list,     V, {LS, _, LDF}) -> datatypes:len_dec_list(V, LDF, LS).
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