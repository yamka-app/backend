-module(tasty_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    {ok, {{one_for_one, 5, 60},
        [{tasty,
            {tasty, start, []},
            permanent,
            5000,
            worker,
            [tasty]}]}}.