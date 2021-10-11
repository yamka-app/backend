%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(entity).
-author("Yamka").
-license("MPL-2.0").

-include("entity.hrl").

-export([encode_field/2, encode/2, len_decode/2, check_excessivity/1]).
-export([get_record/2]).
-export([construct_kv_str/1, filter/2]).

get_record(Type, Id) ->
    Module = list_to_existing_atom(atom_to_list(Type) ++ "_e"),
    #entity{type=Type, fields=(Module):get(Id)}.

structure(Proto, EntityType) ->
    #{Proto := #{EntityType := Structure}} = ?ENTITY_STRUCTURE,
    Structure.

%% encodes entities
encode_field(_Proto, number,   V, {Size})       -> datatypes:enc_num(V, Size);
encode_field(_Proto, string,   V, {})           -> datatypes:enc_str(V);
encode_field(_Proto, string,   V, {_})          -> datatypes:enc_str(V);
encode_field(_Proto, atom,     V, {Size, Map})  -> datatypes:enc_num(maps:get(V, utils:swap_map(Map)), Size);
encode_field(_Proto, bool,     V, {})           -> datatypes:enc_bool(V);
encode_field(_Proto, num_list, V, {Size})       -> datatypes:enc_num_list(V, Size);
encode_field(_Proto, str_list, V, {})           -> datatypes:enc_list(V, fun datatypes:enc_str/1, 2);
encode_field(_Proto, str_list, V, {_})          -> datatypes:enc_list(V, fun datatypes:enc_str/1, 2);
encode_field(_Proto, list,     V, {LS, EF, _})  -> datatypes:enc_list(V, EF, LS);
encode_field( Proto, entity,   V, {})           -> entity:encode(V, Proto);
encode_field( Proto, entity,   V, {T})          -> #entity{type=T}=V, entity:encode(V, Proto);
encode_field(_Proto, bin,      V, {specified})  -> <<(byte_size(V)):16/unsigned, V/bitstring>>;
encode_field(_Proto, bin,      V, {Size})       -> Size = byte_size(V), V.
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
    Desc = lists:nth(1, [D || {I, _, _} = D <- utils:map_keys(RevS), I =:= Id]),
    Name = maps:get(Desc, RevS),
    {Name, Desc}.

%% decodes entities
len_decode_field(_Proto, number,   V, {Size})       -> {datatypes:dec_num(V, Size), Size};
len_decode_field(_Proto, string,   V, {})           -> {datatypes:dec_str(V), datatypes:len_str(V)};
len_decode_field(_Proto, string,   V, {MaxLen})     ->
    S=datatypes:dec_str(V), {if length(S) >= MaxLen -> excessive; true -> S end, datatypes:len_str(V)};
len_decode_field(_Proto, atom,     V, {Size, Map})  -> {maps:get(datatypes:dec_num(V, Size), Map), Size};
len_decode_field(_Proto, bool,     V, {})           -> {datatypes:dec_bool(V), 1};
len_decode_field(_Proto, num_list, V, {Size})       -> R=datatypes:dec_num_list(V, Size), {R, 2+(length(R)*Size)};
len_decode_field(_Proto, str_list, V, {})           -> datatypes:len_dec_list(V, fun datatypes:len_dec_str/1, 2);
len_decode_field(_Proto, str_list, V, {MaxLen})     ->
    {L, Len} = datatypes:len_dec_list(V, fun datatypes:len_dec_str/1, 2),
    Excessive = lists:any(fun(X) -> length(X) >= MaxLen end, L),
    {if Excessive -> excessive; true -> L end, Len};
len_decode_field(_Proto, list,     V, {LS, _, LDF}) -> datatypes:len_dec_list(V, LDF, LS);
len_decode_field( Proto, entity,   V, {})           -> entity:len_decode(V, Proto);
len_decode_field( Proto, entity,   V, {T})          -> {#entity{type=T},_}=entity:len_decode(V, Proto);
len_decode_field(_Proto, bin,      V, {specified})  -> <<Len:16/unsigned, Data/bitstring>> = V, {binary:part(Data, 0, Len), Len + 2};
len_decode_field(_Proto, bin,      V, {Size})       -> {binary_part(V, 0, Size), Size}.
len_decode_field(RevStructure, Bin, Proto) ->
    <<Id:8/unsigned, Repr/binary>> = Bin,
    % find the descriptor
    {Name, {Id, Type, Args}} = find_desc(RevStructure, Id),
    {FieldVal, Len} = len_decode_field(Proto, Type, Repr, Args),
    {{Name, FieldVal}, Len + 1}. % +1 because we have a one byte field ID

len_decode(Bin, Proto) ->
    <<TypeNum:8/unsigned, FieldsBin/binary>> = Bin,
    Type = maps:get(TypeNum, ?ENTITY_TYPE_MAP),
    RevStructure = utils:swap_map(structure(Proto, Type)),
    {FieldProplist, Len} = datatypes:len_dec_list(FieldsBin,
        fun(B) -> len_decode_field(RevStructure, B, Proto) end, 1),
    Fields = maps:from_list(FieldProplist),

    {#entity{type=Type, fields=Fields}, Len + 2}.

check_excessivity(#entity{fields=Fields}) ->
    lists:any(fun(V) -> V =:= excessive end, maps:values(Fields));
check_excessivity(List=[_|_]) ->
    lists:any(fun(X) -> check_excessivity(X) end, List).

%% constructs a "key1=?,key2=? ..." string and a list of cqerl bindings from #{key1=>val1, key2=>val2, ...}
construct_kv_str(Map) when is_map(Map) -> construct_kv_str(maps:to_list(Map));
construct_kv_str([]) -> {"", []};
construct_kv_str([{K,V}|T]) ->
    {RestStr, RestBind} = construct_kv_str(T),
    {atom_to_list(K) ++ "=?" ++ case RestStr of""->"";_->","end ++ RestStr, [{K,V}|RestBind]}.