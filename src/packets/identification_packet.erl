%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(identification_packet).
-author("Order").
-license("MPL-2.0").

-export([decode/2]).

decode(Payload, _Proto) ->
    5 = byte_size(Payload),
    #{
        protocol      => datatypes:dec_num(binary:part(Payload, 0, 4)),
        supports_comp => datatypes:dec_bool(binary:part(Payload, 4, 1))
    }.