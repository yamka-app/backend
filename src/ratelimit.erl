-module(ratelimit).
-author("Order").
-license("MPL-2.0").
-description("Rate limiting functionality").

-record(limiter, { hits_in_win, cur_win, hits_per_win, win_size }).

-export([make/2, reset/1, hit/2, hit/1]).

%% gets (or makes, if called for the first time) a limit table
get_limit_table() ->
    case get(limiter_tabid) of
        undefined ->
            Tid = ets:new(limiter_tab, [set]),
            put(limiter_tabid, Tid), Tid;
        T -> T
    end.

%% makes a limiter and binds it to the process
make(Name, { HitsPerWindow, WindowSize }) ->
    Tab = get_limit_table(),
    % insert a limiter
    ets:insert(Tab, { Name, #limiter{
        hits_in_win  = 0,
        hits_per_win = HitsPerWindow,
        win_size     = WindowSize,
        cur_win      = 0
    } }).

%% calculates the window ID
win_id(Size) ->
    { MeS, S, _MiS } = now(),
    ((MeS * 1000000) + S) div Size.

%% resets a limiter
reset(Name) ->
    Tab = get_limit_table(),
    [{ Name, Limiter }] = ets:lookup(Tab, Name),
    Limiter#limiter{ cur_win = win_id(Limiter#limiter.win_size), hits_in_win = 0 },
    ets:insert(Tab, { Name, Limiter }).

%% hits a limiter and checks if an action is allowed
hit(Name, 1) ->
    Tab = get_limit_table(),
    [{ Name, Limiter }] = ets:lookup(Tab, Name),
    % get the window
    Win = win_id(Limiter#limiter.win_size),
    % check it against the current window
    NewHits = if
        Win == Limiter#limiter.cur_win -> Limiter#limiter.hits_in_win + 1;
        true -> 1
    end,
    % write the new hit count and window ID
    UpdLimiter = Limiter#limiter{ cur_win = Win, hits_in_win = NewHits },
    ets:insert(Tab, { Name, UpdLimiter }),
    % check the number of hits
    if
        NewHits > Limiter#limiter.hits_per_win -> 0;
        true -> 1
    end;

hit(Name, N) when N > 1 -> hit(Name, 1) + hit(Name, N - 1).

hit(Name) -> hit(Name, 1).