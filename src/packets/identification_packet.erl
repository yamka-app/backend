%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(identification_packet).
-author("Yamka").
-license("MPL-2.0").

-export([decode/2]).

decode(<<Protocol:32/unsigned, Comp:8/integer>>, _Proto) ->
    #{
        protocol      => Protocol,
        supports_comp => Comp > 0
    }.