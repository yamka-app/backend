-module(logging).
-author("Order").
-license("MPL-2.0").
-description("Logs stuff").

-export([generic/3,
         err/2, warn/2, log/2]).

generic(Level, Msg, Args) ->
    D = date(),
    T = time(),
    io:fwrite("[~w/~w/~w ~w:~w][~c] ", [
        element(1, D), element(2, D), element(3, D),
        element(1, T), element(2, T),
        Level
       ]),
    io:fwrite(Msg, Args),
    io:fwrite("~n").

err (Msg, Args) -> generic($E, Msg, Args).
warn(Msg, Args) -> generic($W, Msg, Args).
log (Msg, Args) -> generic($L, Msg, Args).