%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(voice_join_packet).
-author("Yamka").
-license("MPL-2.0").

-include("packet.hrl").
-export([encode/2, decode/2, make/3]).

encode(#{crypto:=Session, address:=Addr}, Proto) when Proto >= 5 ->
    AddrBin = datatypes:enc_str(Addr),
    <<0:64/unsigned, AddrBin/binary, Session/binary>>.

%% the two zero bytes specify an empty string
%% why is there always an empty string?
%% I'm just too lazy to implement different structures for a packet
%% on the client side or two different packets here
decode(<<Channel:64/unsigned, 0:16/integer, Key:256/bitstring>>, ProtocolVersion) when ProtocolVersion >= 5 ->
    #{channel => Channel, crypto => Key}.

make(S, A, R) -> #packet{type = voice_join, reply = R,
    fields = #{crypto => S, address => A}}.