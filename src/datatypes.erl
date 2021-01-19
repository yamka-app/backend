-module(datatypes).
-author("Order").
-license("MPL-2.0").
-description("Basic binary data conversion").

-export([enc_num/2,  dec_num/1, dec_num/2,
         enc_bool/1, dec_bool/1,
         enc_str/1,  dec_str/1,  len_str/1,
         enc_list/3,
         dec_list/3, dec_list/4, len_dec_list/3, len_list/3,
         enc_num_list/2, dec_num_list/2]).

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

len_dec_items(_, _, 0) -> { [], 0 };
len_dec_items(Bin, LengthingDecoder, Cnt) when Cnt > 0 ->
   { Len, Val } = LengthingDecoder(Bin),
   { Tail, OtherLen } = len_dec_items(binary:part(Bin, Len, byte_size(Bin) - Len), LengthingDecoder, Cnt - 1),
   { [Val | Tail], OtherLen + Len }.
len_dec_list(Bin, LengthingDecoder, CountLength) ->
   Cnt = dec_num(binary:part(Bin, 0, CountLength)),
   Rest = binary:part(Bin, CountLength, byte_size(Bin) - CountLength),
   len_dec_items(Rest, LengthingDecoder, Cnt).

dec_list(Bin, LengthingDecoder, CountLength) ->
   { Val, _ } = len_dec_list(Bin, LengthingDecoder, CountLength),
   Val.
dec_list(Bin, Decoder, Lengther, CountLength) ->
   { Val, _ } = len_dec_list(Bin, fun(B)->{Decoder(B), Lengther(B)} end, CountLength),
   Val.

len_list(Bin, Lengther, CountLength) ->
   { _, Len } = len_dec_list(Bin, fun(B)->{fun(_)->none end, Lengther(B)} end, CountLength),
   Len.

enc_num_list(List, Bytes) -> enc_list(List, fun(X)->enc_num(X, Bytes) end, 2).
dec_num_list(List, Bytes) -> dec_list(List, fun(X)->{ dec_num(X), Bytes } end, 2).