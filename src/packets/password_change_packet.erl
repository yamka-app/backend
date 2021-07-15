%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(password_change_packet).
-author("Yamka").
-license("MPL-2.0").

-include("packet.hrl").
-export([decode/2]).

decode(<<Payload/binary>>, Proto) when Proto >= 11 ->
    Len = byte_size(Payload),

    OldPass    = datatypes:dec_str(Payload),
    OldPassLen = datatypes:len_str(Payload),

    MFACodeBin  = binary:part(Payload, OldPassLen, Len - OldPassLen),
    MFACode     = datatypes:dec_str(MFACodeBin),
    MFACodeLen  = datatypes:len_str(MFACodeBin),

    PassBin  = binary:part(Payload, OldPassLen+MFACodeLen, Len-OldPassLen-MFACodeLen),
    Pass     = datatypes:dec_str(PassBin),
    PassLen  = datatypes:len_str(PassBin),

    #{old_pass => OldPass, mfa_code => MFACode, pass => Pass}.