-module(user_search_packet).
-author("Order").
-license("MPL-2.0").

-include("packet.hrl").
-export([decode/2]).

decode(Name, ProtocolVersion) when ProtocolVersion >= 5 ->
    #{ name => datatypes:dec_str(Name) }.