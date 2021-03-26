%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(signup_packet).
-author("Order").
-license("MPL-2.0").

-export([decode/2]).

decode(Payload, ProtocolVersion) when ProtocolVersion >= 5 ->
    Email    = datatypes:dec_str(Payload),
    EmailLen = datatypes:len_str(Payload),

    NameBin  = binary:part(Payload, EmailLen, byte_size(Payload) - EmailLen),
    Name     = datatypes:dec_str(NameBin),
    NameLen  = datatypes:len_str(NameBin),

    PassBin  = binary:part(Payload, EmailLen+NameLen, byte_size(Payload)-EmailLen-NameLen),
    Pass     = datatypes:dec_str(PassBin),
    NameLen  = datatypes:len_str(NameBin),
    #{email => Email, name => Name, password => Pass}.