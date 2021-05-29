%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(signup_packet).
-author("Yamka").
-license("MPL-2.0").

-include("../entities/entity.hrl").

-export([decode/2]).

decode(Payload, ProtocolVersion) when ProtocolVersion >= 9 ->
    Len = byte_size(Payload),

    Email    = datatypes:dec_str(Payload),
    EmailLen = datatypes:len_str(Payload),

    NameBin  = binary:part(Payload, EmailLen, Len - EmailLen),
    Name     = datatypes:dec_str(NameBin),
    NameLen  = datatypes:len_str(NameBin),

    PassBin  = binary:part(Payload, EmailLen+NameLen, Len-EmailLen-NameLen),
    Pass     = datatypes:dec_str(PassBin),
    PassLen  = datatypes:len_str(PassBin),

    AgentBin = binary:part(Payload, EmailLen+NameLen+PassLen, Len-EmailLen-NameLen-PassLen),
    {Agent = #entity{type=agent}, _} = entity:len_decode(AgentBin, ProtocolVersion),

    #{email => Email, name => Name, password => Pass, agent => Agent}.