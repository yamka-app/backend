-module(datatypes).
-author("Order").
-license("MPL-2.0").
-description("Basic binary data conversion").

-export([enc_num/2,  dec_num/1,
         enc_bool/1, dec_bool/1]).

bconcat(A, B) -> <<A/binary, B/binary>>.
bpad(r, B, 0) -> B;
bpad(r, B, L) -> bconcat(<<0>>, bpad(r, B, L - 1)).
bpad(B, L)    -> bpad(r, B, L - byte_size(B)).

enc_num(X, L) -> bpad(binary:encode_unsigned(X, big), L).
dec_num(X)    -> binary:decode_unsigned(X, big).

enc_bool(true)  -> enc_num(1, 1);
enc_bool(false) -> enc_num(0, 1).
dec_bool(X) ->
    D = dec_num(X),
    if D == 0 -> false;
       true -> true
    end.