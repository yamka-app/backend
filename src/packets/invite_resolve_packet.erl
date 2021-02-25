-module(invite_resolve_packet).
-author("Order").
-license("MPL-2.0").

-export([decode/2]).

decode(<<Add:8/unsigned-integer, Code:112/bitstring>>, Proto) when Proto >= 5 ->
    #{code => datatypes:dec_str(Code), add => Add > 0}.