%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(entity_get_packet).
-author("Yamka").
-license("MPL-2.0").

-include("packet.hrl").
-include("../entities/entity.hrl").
-export([decode/2]).

len_segment(<<_:77, K:1/unsigned, C:1/unsigned, P:1/unsigned, _/bitstring>>) -> 10 + K + (C * 9) + (P * 11).

decode_pagination(<<Field:8/unsigned, Dir:8/unsigned, From:64/unsigned, Cnt:8/unsigned>>) ->
    DirAtom = case Dir of
        0 -> down;
        1 -> up
    end,
    #entity_pagination{field=Field, dir=DirAtom, from=From, cnt=Cnt}.
decode_context(<<Type:8/unsigned, Id:64/unsigned>>) ->
    #entity_context{type=maps:get(Type, ?ENTITY_TYPE_MAP), id=Id}.
decode_key(<<Type:8/unsigned>>) -> maps:get(Type, ?KEY_TYPE_MAP).

decode_segment(Bin) ->
    <<Type:8/unsigned, Id:64/unsigned, _:5, K:1/unsigned, C:1/unsigned, P:1/unsigned, PCKBin/binary>> = Bin,
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
    #entity_get_rq{type=maps:get(Type, ?ENTITY_TYPE_MAP), id=Id, pagination=Pagination, context=Context, key=Key}.

decode(P, Proto) when Proto >= 5 ->
    #{entities => datatypes:dec_list(P, fun decode_segment/1, fun len_segment/1, 2)}.