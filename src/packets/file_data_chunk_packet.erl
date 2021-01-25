-module(file_data_chunk_packet).
-author("Order").
-license("MPL-2.0").

-include("packet.hrl").
-export([encode/2, decode/2, make/3]).

encode(#{position:=Pos, data:=Data}, Proto) when Proto >= 5 ->
    <<Pos:32/unsigned-integer, Data/binary>>.

decode(<<Pos:32/unsigned-integer, Data/binary>>, ProtocolVersion) when ProtocolVersion >= 5 ->
    #{position => Pos, data => Data}.

make(P, D, R) -> #packet{type = file_data_chunk, reply = R, fields = #{position => P, data => D}}.