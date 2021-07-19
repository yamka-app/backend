%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(entity_get_packet).
-author("Yamka").
-license("MPL-2.0").

-include("../entities/entity.hrl").
-export([decode/2]).

len_segment(<<_:76, _D:1, K:1, C:1, P:1, _/bitstring>>) -> 10 + K + (C * 9) + (P * 11).

decode_pagination(<<Field:8, Dir:8, From:64, Cnt:8>>) ->
    DirAtom = case Dir of
        0 -> down;
        1 -> up
    end,
    #entity_pagination{field=Field, dir=DirAtom, from=From, cnt=Cnt}.
decode_context(<<Type:8, Id:64>>) ->
    #entity_context{type=maps:get(Type, ?ENTITY_TYPE_MAP), id=Id}.
decode_key(<<Type:8>>) -> maps:get(Type, ?KEY_TYPE_MAP).

decode_segment(<<Type:8, Id:64, _:4, D:1, K:1, C:1, P:1, PCKBin/bitstring>>) ->
    % D = In-Place Dereference (valid in combination with P)
    % K = Key
    % C = Context
    % P = Pagination
    {Pagination, CKBin} = case P of
        0 -> {none, PCKBin};
        1 -> <<P_Bin:88/bitstring, Rest/bitstring>> = PCKBin,
            {decode_pagination(P_Bin), Rest}
    end,
    {Context, KBin} = case C of
        0 -> {none, CKBin};
        1 -> <<C_Bin:72/bitstring, Rest2/bitstring>> = CKBin,
            {decode_context(C_Bin), Rest2}
    end,
    Key = case K of
        0 -> none;
        1 -> decode_key(KBin)
    end,
    #entity_get_rq{type=maps:get(Type, ?ENTITY_TYPE_MAP), id=Id,
        pagination = Pagination,
        context = Context,
        key = Key,
        deref = D =:= 1}.

decode(P, Proto) when Proto >= 5 ->
    #{entities => datatypes:dec_list(P, fun decode_segment/1, fun len_segment/1, 2)}.