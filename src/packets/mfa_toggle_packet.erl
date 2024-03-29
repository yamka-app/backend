%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(mfa_toggle_packet).
-author("Yamka").
-license("MPL-2.0").

-export([decode/2]).

decode(<<EnableBin, PasswordBin/binary>>, Proto) when Proto >= 12 ->
    #{enable => EnableBin >= 1,
      pass   => datatypes:dec_str(PasswordBin)}.