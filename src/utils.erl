%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(utils).
-author("Yamka").
-license("MPL-2.0").
-description("Different utilities").

-export([swap_map/1, map_keys/1, intersect_lists/1,
         hash_token/1, hash_password/2,
         gen_snowflake/0, gen_invite/0, gen_avatar/0,
         temp_file_name/0]).
-export([broadcast/2, safe_call/2, safe_call/3]).
-export([ms_since/1]).
-export([list_diff/2]).

%% broadcasts some value to a list of processes
broadcast(_, []) -> ok;
broadcast(D, [H|T]) -> H ! D, broadcast(D, T).

%% swaps map keys and values
swap_map(M) -> maps:from_list([{V,K} || {K,V} <- maps:to_list(M)]).

%% returns a list of map keys
map_keys(M) -> [K || {K,_} <- maps:to_list(M)].

%% intersects lists
intersect_lists(LL) -> sets:to_list(sets:intersection([sets:from_list(L) || L <- LL])).

%% hashes a token the way it's stored in the database
hash_token(T, 0) -> crypto:hash(sha512, T);
hash_token(T, R) when R > 0 ->
    H = hash_token(T, R - 1),
    crypto:hash(sha512, <<T/binary, H/binary>>).
hash_token(T) -> hash_token(T, 64).

%% hashes a password the way it's stored in the database
hash_password(Pass, Salt) ->
    B = unicode:characters_to_binary(Pass),
    hash_token(<<B/binary, Salt/binary>>, 64).

%% generates an ID (Snowflake)
gen_snowflake() ->
    {MeS, S, MiS} = now(),
    Epoch = ((MeS * 1000000000) + (S * 1000) + (MiS div 1000)) - 1577836800000,
    Random = crypto:strong_rand_bytes(2),
    <<Snowflake:64/unsigned-integer>> = <<Epoch:48/unsigned-integer, Random/binary>>,
    Snowflake.

%% generates an invite code
gen_invite() -> base64:encode(crypto:strong_rand_bytes(9)).

%% generates a temporary file
temp_file_name() ->
    {MS, S, MiS} = now(),
    lists:flatten(io_lib:format("/tmp/~p.~p.~p", [MS, S, MiS])).

%% gen_avatar helper: expands a line into superpixels horizontally
expand_line_h(L, SS) -> lists:flatten([[X || _ <- lists:seq(1, SS)] || X <- L]).
%% gen_avatar helper: mirrors a pattern around the Y axis
mirror_line_h(L) -> lists:append(L, tl(lists:reverse(L))).
%% generates an avatar
gen_avatar() ->
    {Width, Height} = {280, 280}, Width = Height,
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
    Filename = temp_file_name(),
    {ok, File} = file:open(Filename, [write]),
    % create the image
    Png = png:create(#{size    => {Width, Height},
                        mode    => {rgba, 8},
                        file    => File}),
    
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
        png:append(Png, {row, Row}) end,

    lists:foreach(AppendRow, lists:seq(1, Height)),

    % flush the data
    ok = png:close(Png),
    
    Filename.

%% calls the function safely
put_pd([]) -> ok;
put_pd([{K,V}|T]) -> put(K, V), put_pd(T).

safe_call(Fun, Args) -> safe_call(Fun, Args, []).
safe_call(Fun, Args, PD) ->
    Self = self(),
    Wrapper = fun() ->
            put_pd(PD),
            Self ! {ok, self(), apply(Fun, Args)}
        end,
    {Pid, _} = spawn_monitor(Wrapper),
    receive
        {ok, Pid, Val} -> {ok, Val};
        {'DOWN', _, process, Pid, Reason} -> {error, Reason}
    end.

%% time difference between now and past in ms
ms_since(Time) -> erlang:convert_time_unit(erlang:monotonic_time() - Time, native, milli_seconds).

%% difference between two lists
list_diff(L1, L2) ->
    {
        lists:filter(fun(V) -> not lists:member(V, L2) end, L1),
        lists:filter(fun(V) -> not lists:member(V, L1) end, L2)
    }.