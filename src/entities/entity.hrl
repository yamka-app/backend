-record(entity, {type=unknown, fields=#{}}).
-record(entity_pagination, {field=0, dir=unknown, from=0, cnt=0}).
-record(entity_context, {type=unknown, id=0}).
-record(entity_get_rq, {type=unknown, id=0, pagination=none, context=none}).

-define(REVERSE_ENTITY_MAP, utils:swap_map(?ENTITY_TYPE_MAP)).
-define(ENTITY_TYPE_MAP, #{
    1 => user,
    2 => channel,
    3 => group,
    4 => message,
    5 => role,
    6 => file
}).

-define(USER_STATUS_MAP, #{0=>offline, 1=>online, 2=>idle, 3=>dnd}).
-define(ENTITY_STRUCTURE, #{
    user => #{
            id          => {0,  number,   {8}},
            email       => {1,  string,   {}},
            name        => {2,  string,   {}},
            tag         => {3,  number,   {3}},
            status      => {4,  atom,     {1, ?USER_STATUS_MAP}},
            status_text => {5,  string,   {}},
            ava_file    => {7,  number,   {8}},
            mfa_enabled => {8,  bool,     {}},
            friends     => {9,  num_list, {8}},
            blocked     => {10, num_list, {8}},
            pending_in  => {11, num_list, {8}},
            pending_out => {12, num_list, {8}},
            dm_channels => {13, num_list, {8}},
            groups      => {14, num_list, {8}},
            roles       => {15, num_list, {8}},
            color       => {16, number,   {3}},
            badges      => {17, num_list, {1}},
            bot_owner   => {18, number,   {8}},
            owned_bots  => {19, num_list, {8}}
       }
}).