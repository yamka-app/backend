-module(main).
-author("Order").
-license("MPL-2.0").
-description("The main file").

-export([start/0]).

start() ->
    ssl:start(),
    spawn(listeners, normal_listener, [
        os:getenv("CAS_LOGIN"),
        os:getenv("CAS_PASS"),
        os:getenv("CERT_PATH"),
        os:getenv("KEY_PATH")
       ]),

    receive
        die -> void
    end.