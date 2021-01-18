-module(entity).
-author("Order").
-license("MPL-2.0").

-include("entity.hrl").

-export([handle_get_request/1]).
-export([encode_field/3, encode_field/1, encode/1]).

%% handles a get request
handle_get_request(#entity_get_rq{ type=user, id=Id, pagination=none, context=none }) ->
    CallerId = get(id),
    if Id == CallerId; Id == 0 -> true = auth:has_permission(see_profile);
       true -> ok end,

    % get the user and insert default values that might be null
    Fields = user:get(Id),
    FieldsWithDefVals = maps:merge(#{
        friends     => [],
        blocked     => [],
        pending_in  => [],
        pending_out => [],
        dm_channels => [],
        groups      => [],
        badges      => []
    }, maps:filter(fun(_, V) -> V /= null end, Fields)),

    % filter fields based on permissions
    FilteredFields = maps:filter(fun(K, _) ->
        case K of
            id          -> true;
            email       -> Id == get(id);
            name        -> true;
            tag         -> true;
            status      -> true;
            status_text -> true;
            ava_file    -> true;
            friends     -> auth:has_permission(see_relationships);
            blocked     -> auth:has_permission(see_relationships);
            pending_in  -> auth:has_permission(see_relationships);
            pending_out -> auth:has_permission(see_relationships);
            dm_channels -> auth:has_permission(see_direct_messages);
            groups      -> auth:has_permission(see_groups);
            badges      -> true;
            bot_owner   -> true;
            _ -> false
        end
    end, FieldsWithDefVals),
    #entity{ type=user, fields=FilteredFields }.

%% encodes entities
encode_field(number,   V, { Size })      -> datatypes:enc_num(V, Size);
encode_field(string,   V, {})            -> datatypes:enc_str(V);
encode_field(atom,     V, { Size, Map }) -> datatypes:enc_num(maps:get(V, utils:swap_map(Map)), Size);
encode_field(bool,     V, {})            -> datatypes:enc_bool(V);
encode_field(num_list, V, { Size })      -> datatypes:enc_num_list(V, Size).
encode_field({ { Id, Type, Args }, Value }) ->
    Repr = encode_field(Type, Value, Args),
    IdBin = datatypes:enc_num(Id, 1),
    <<IdBin/binary, Repr/binary>>.

encode(#entity{ type=Type, fields=Fields }) ->
    TypeBin        = datatypes:enc_num(maps:get(Type, ?REVERSE_ENTITY_MAP), 1),
    Structure      = maps:get(Type, ?ENTITY_STRUCTURE),

    FieldsToEncode = utils:intersect_lists([utils:map_keys(Fields), utils:map_keys(Structure)]),
    FieldValues    = [{ maps:get(K, Structure), maps:get(K, Fields) } || K <- FieldsToEncode],

    BinaryRepr     = datatypes:enc_list(FieldValues, fun encode_field/1, 1),
    <<TypeBin/binary, BinaryRepr/binary>>.