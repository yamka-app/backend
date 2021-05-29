%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(invite_resolve_packet).
-author("Yamka").
-license("MPL-2.0").

-export([decode/2]).

decode(<<Add:8/unsigned, Code:112/bitstring>>, Proto) when Proto >= 5 ->
    #{code => datatypes:dec_str(Code), add => Add > 0}.