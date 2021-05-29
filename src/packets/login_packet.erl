%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(login_packet).
-author("Yamka").
-license("MPL-2.0").

-include("packet.hrl").
-include("../entities/entity.hrl").

-export([decode/2]).

decode(Payload, ProtocolVersion) when ProtocolVersion >= 9 ->
    Len = byte_size(Payload),

    Email    = datatypes:dec_str(Payload),
    EmailLen = datatypes:len_str(Payload),

    PasswordBin = binary:part(Payload, EmailLen, Len-EmailLen),
    Password    = datatypes:dec_str(PasswordBin),
    PasswordLen = datatypes:len_str(PasswordBin),

    PermsBin = binary:part(Payload, EmailLen+PasswordLen, Len-EmailLen-PasswordLen),
    PermsNum = datatypes:dec_num_list(PermsBin, 1),
    Perms    = [maps:get(C, ?TOKEN_PERMISSION_MAP) || C <- PermsNum],
    PermsLen = length(Perms) + 2,

    AgentBin = binary:part(Payload, EmailLen+PasswordLen+PermsLen, Len-EmailLen-PasswordLen-PermsLen),
    {Agent = #entity{type=agent}, _} = entity:len_decode(AgentBin, ProtocolVersion),

    #{email => Email, password => Password, perms => Perms, agent => Agent}.