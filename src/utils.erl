-module(utils).
-author("Order").
-license("MPL-2.0").
-description("Different utilities").

-export([hash_password/2, gen_snowflake/0]).

%% hashes a password the way it's stored in the database
hash_password(Pass, Salt, 0) -> crypto:hash(sha512, <<Pass/binary, Salt/binary>>);
hash_password(Pass, Salt, R) when R > 0 ->
    H = hash_password(Pass, Salt, R - 1),
    crypto:hash(sha512, <<Pass/binary, Salt/binary, H/binary>>).
hash_password(Pass, Salt) -> hash_password(unicode:characters_to_binary(Pass), Salt, 64).

%% generates an ID (Snowflake)
gen_snowflake() ->
    { MeS, S, _MiS } = now(),
    Epoch = ((MeS * 1000000) + S),
    Random = crypto:strong_rand_bytes(2),
    <<Epoch:48/unsigned-integer, Random/binary>>.