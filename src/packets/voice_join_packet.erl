-module(voice_join_packet).
-author("Order").
-license("MPL-2.0").

-include("packet.hrl").
-export([encode/2, decode/2, make/2]).

encode(#{crypto:=Session}, Proto) when Proto >= 5 ->
    <<0:64/unsigned-integer, Session/binary>>.

decode(<<Channel:64/unsigned-integer, Key:128/binary>>, ProtocolVersion) when ProtocolVersion >= 5 ->
    #{channel => Channel, crypto => Key}.

make(S, R) -> #packet{type = file_data_chunk, reply = R, fields = #{crypto => S}}.