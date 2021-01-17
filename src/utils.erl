-module(utils).
-author("Order").
-license("MPL-2.0").
-description("Different utilities").

-export([hash_password/2, gen_snowflake/0, gen_avatar/0]).

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
    <<Snowflake:64/unsigned-integer>> = <<Epoch:48/unsigned-integer, Random/binary>>, Snowflake.

%% gen_avatar helper: expands a line into superpixels horizontally
expand_line_h(L, SS) -> lists:flatten([[X || _ <- lists:seq(1, SS)] || X <- L]).
%% gen_avatar helper: mirrors a pattern around the Y axis
mirror_line_h(L) -> lists:append(L, tl(lists:reverse(L))).
%% generates an avatar
gen_avatar() ->
    { Width, Height } = { 140, 140 }, Width = Height,
    SpixelSz = Width div 7,

    Palette = [
        [<<232,  76,  61, 255>>, <<201,  66,  53, 255>>], % 1st pair
        [<<247, 204,   0, 255>>, <<217, 179,   0, 255>>], % 2nd pair
        [<< 45, 204, 112, 255>>, << 39, 174,  97, 255>>], % ...
        [<< 51, 200, 188, 255>>, << 43, 169, 159, 255>>],
        [<<112,  62, 244, 255>>, << 98,  54, 213, 255>>],
        [<<188, 135, 177, 255>>, <<157, 113, 148, 255>>],
        [<<244,  62, 131, 255>>, <<213,  54, 115, 255>>]
    ],
    % create the file
    { MS, S, MiS } = now(),
    RandomFilename = lists:flatten(io_lib:format("/tmp/~p.~p.~p", [MS, S, MiS])),
    { ok, File } = file:open(RandomFilename, [write]),
    % create the image
    Png = png:create(#{ size    => { Width, Height },
                        mode    => { rgba, 8 },
                        file    => File }),
    
    % make the image
    Pair = lists:nth(rand:uniform(7), Palette),
    Pattern = mirror_line_h([rand:uniform(3) - 1 || _ <- lists:seq(1, 4)]),
    InversePattern = [2 - P || P <- Pattern],

    AppendRow = fun(Y) ->
        Line = Y div SpixelSz,
        UsingPattern = case Line of
            0 -> Pattern;
            1 -> InversePattern;
            2 -> InversePattern;
            3 -> Pattern;
            4 -> InversePattern;
            5 -> Pattern;
            _ -> Pattern
        end,
        Colors = [if P == 0 -> <<0,0,0,0>>; true -> lists:nth(P, Pair) end || P <- UsingPattern],
        %Colors = [<<255, 0, 0, 255>> || _ <- lists:seq(1, Width)],
        Row = expand_line_h(Colors, SpixelSz),
        png:append(Png, { row, Row }) end,

    lists:foreach(AppendRow, lists:seq(1, Height)),

    % flush the data
    ok = png:close(Png),
    
    RandomFilename.