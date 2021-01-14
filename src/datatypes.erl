-module(datatypes).
-author("Order").
-license("MPL-2.0").
-description("Basic binary data conversion").

-export([enc_num/2,  dec_num/1,
         enc_bool/1, dec_bool/1,
         enc_str/1,  dec_str/1,  len_str/1]).

bconcat(A, B) -> <<A/binary, B/binary>>.
bpad(r, B, 0) -> B;
bpad(r, B, L) -> bconcat(<<0>>, bpad(r, B, L - 1)).
bpad(B, L)    -> bpad(r, B, L - byte_size(B)).

enc_num(X, L) -> bpad(binary:encode_unsigned(X, big), L).
dec_num(X)    -> binary:decode_unsigned(X, big).

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