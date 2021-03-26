%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(datatypes).
-author("Order").
-license("MPL-2.0").
-description("Basic binary data conversion").

-include("entities/entity.hrl").

-export([enc_num/2,  dec_num/1, dec_num/2,
         enc_bool/1, dec_bool/1,
         enc_str/1,  dec_str/1,  len_str/1, len_dec_str/1,
         enc_list/3,
         dec_list/3, dec_list/4, len_dec_list/3, len_list/3,
         enc_num_list/2, dec_num_list/2]).
-export([enc_msg_section/1, len_dec_msg_section/1]).
-export([enc_chan_voice_status/1, len_dec_chan_voice_status/1]).

bconcat(A, B) -> <<A/binary, B/binary>>.
bpad(r, B, 0) -> B;
bpad(r, B, L) -> bconcat(<<0>>, bpad(r, B, L - 1)).
bpad(B, L)    -> bpad(r, B, L - byte_size(B)).

enc_num(X, L) -> bpad(binary:encode_unsigned(X, big), L).
dec_num(X)    -> binary:decode_unsigned(X, big).
dec_num(X, S) -> dec_num(binary:part(X, 0, S)).

enc_bool(true)  -> enc_num(1, 1);
enc_bool(false) -> enc_num(0, 1).
dec_bool(X)     -> dec_num(X) > 0.

enc_str(X) ->
   Bin = unicode:characters_to_binary(X),
   Len = enc_num(byte_size(Bin), 2),
   <<Len/binary, Bin/binary>>.
dec_str(X) ->
   % len_str/1 counts the length marker too, we don't need that here
   Len = len_str(X) - 2,
   Bin = binary:part(X, 2, Len),
   unicode:characters_to_list(Bin).
len_str(X) ->
   % take the length marker into account
   dec_num(binary:part(X, 0, 2)) + 2.
len_dec_str(X) -> {dec_str(X), len_str(X)}.

enc_msg_section(#message_section{type=Type, blob=Blob, text=Text}) ->
   TypeNum = maps:get(Type, utils:swap_map(?MESSAGE_SECTION_TYPE_MAP)),
   Str = enc_str(Text),
   <<TypeNum:8/unsigned-integer, Blob:64/unsigned-integer, Str/binary>>.
len_dec_msg_section(<<TypeNum:8/unsigned-integer, Blob:64/unsigned-integer, Str/binary>>) ->
   Type = maps:get(TypeNum, ?MESSAGE_SECTION_TYPE_MAP),
   Text = dec_str(Str),
   {#message_section{type=Type, blob=Blob, text=Text}, len_str(Str) + 9}.

num_enc_chan_voice_status([]) -> 0;
num_enc_chan_voice_status([H|T]) ->
   case H of
      speaking -> 1;
      muted    -> 2;
      deafened -> 4
   end
      bor num_enc_chan_voice_status(T).
enc_chan_voice_status(List) ->
   Num = num_enc_chan_voice_status(List),
   <<Num>>.

status_bit_at(7) -> speaking;
status_bit_at(6) -> muted;
status_bit_at(5) -> deafened.
decode_status_bits(<<>>, 8) -> [];
decode_status_bits(<<0:1, Rest/bitstring>>, C) ->
   decode_status_bits(Rest, C + 1);
decode_status_bits(<<1:1, Rest/bitstring>>, C) ->
   [status_bit_at(C)|decode_status_bits(Rest, C + 1)].
decode_status_bits(<<Bits:8/bitstring>>) -> decode_status_bits(Bits, 0).
len_dec_chan_voice_status(Bits) -> {decode_status_bits(Bits), 1}.


%%% this set of functions expects three functions to work with data types:
%%%  - encoder: encodes a value into its binary form
%%%  - decoder: decodes a value from its binary form
%%%  - lengther: assuming a value starts at the start of the supplied binary,
%%%              determines how long it is
%%%  - lengthing decoder: decodes and determines the length of the supplied binary
enc_items([], _) -> <<>>;
enc_items([H|T], Encoder) ->
   Item = Encoder(H),
   Rest = enc_items(T, Encoder),
   <<Item/binary, Rest/binary>>.
enc_list(List, Encoder, CountLength) ->
   Len = enc_num(length(List), CountLength),
   Items = enc_items(List, Encoder),
   <<Len/binary, Items/binary>>.

len_dec_items(_, _, 0) -> {[], 0};
len_dec_items(Bin, LengthingDecoder, Cnt) when Cnt > 0 ->
   {Val, Len} = LengthingDecoder(Bin),
   {Tail, OtherLen} = len_dec_items(binary:part(Bin, Len, byte_size(Bin) - Len), LengthingDecoder, Cnt - 1),
   {[Val|Tail], OtherLen + Len}.
len_dec_list(Bin, LengthingDecoder, CountLength) ->
   Cnt = dec_num(binary:part(Bin, 0, CountLength)),
   Rest = binary:part(Bin, CountLength, byte_size(Bin) - CountLength),
   len_dec_items(Rest, LengthingDecoder, Cnt).

dec_list(Bin, LengthingDecoder, CountLength) ->
   {Val, _} = len_dec_list(Bin, LengthingDecoder, CountLength),
   Val.
dec_list(Bin, Decoder, Lengther, CountLength) ->
   {Val, _} = len_dec_list(Bin, fun(B)->{Decoder(B), Lengther(B)} end, CountLength),
   Val.

len_list(Bin, Lengther, CountLength) ->
   {_, Len} = len_dec_list(Bin, fun(B)->{fun(_)->none end, Lengther(B)} end, CountLength),
   Len.

enc_num_list(List, Bytes) -> enc_list(List, fun(X)->enc_num(X, Bytes) end, 2).
dec_num_list(List, Bytes) -> dec_list(List, fun(X)->{dec_num(X, Bytes), Bytes} end, 2).