%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(email_confirmation_packet).
-author("Yamka").
-license("MPL-2.0").

-export([decode/2]).

decode(CodeBin, Proto) when Proto >= 5 ->
    Code = datatypes:dec_str(CodeBin),
    #{code => Code}.