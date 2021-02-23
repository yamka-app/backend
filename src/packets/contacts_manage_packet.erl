-module(contacts_manage_packet).
-author("Order").
-license("MPL-2.0").

-include("packet.hrl").
-export([decode/2]).

decode(<<Type:8/unsigned-integer,
         Action:8/unsigned-integer,
         Id:64/unsigned-integer>>, ProtocolVersion) when ProtocolVersion >= 5 ->
    #{
        type   => maps:get(Type, #{0=>friend,1=>blocked,2=>pending_in,3=>pending_out,4=>group}),
        action => maps:get(Action, #{0=>add,1=>remove}),
        id     => Id
    }.