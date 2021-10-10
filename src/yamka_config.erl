%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(yamka_config).
-export([get/1]).

get(Key) ->
    case Key of 
        sweet_port -> 1746;
        sweet_client_timeout -> 30 * 1000;
        sweet_comp_threshold -> 128;
        sweet_protocol_between -> {12, 14};
        sweet_file_chunk_size -> 4096;

        tasty_port -> 1747;
        tasty_client_timeout -> 15 * 1000;
        tasty_packet_rate_limit -> 150;
        tasty_packet_sz_limit -> 128;
        tasty_speaking_ind_threshold -> 250;

        email_relay -> "mail.yamka.app";
        stat_interval -> 30 * 1000;
        token_ttl -> 3600 * 24 * 365;
        cassandra -> {"scylla", 9042};
        typing_reset_threshold -> 15 * 1000;
        file_storage_path -> "/data/brick1/gv0/file_storage/"
    end.