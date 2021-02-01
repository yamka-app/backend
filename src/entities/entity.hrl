-record(entity, {type=unknown, fields=#{}}).
-record(entity_pagination, {field=0, dir=unknown, from=0, cnt=0}).
-record(entity_context, {type=unknown, id=0}).
-record(entity_get_rq, {type=unknown, id=0, pagination=none, context=none}).
-record(message_section, {type=unknown, blob=0, text=""}).

-define(MESSAGE_SECTION_TYPE_MAP, #{
    0 => text,
    1 => file,
    2 => code,
    3 => quote,
    4 => invite,
    5 => user,
    6 => bot_ui
}).

-define(REVERSE_ENTITY_MAP, utils:swap_map(?ENTITY_TYPE_MAP)).
-define(ENTITY_TYPE_MAP, #{
    1 => user,
    2 => channel,
    3 => group,
    4 => message,
    5 => role,
    6 => file,
    7 => message_state
}).

-define(USER_STATUS_MAP, #{0=>offline, 1=>online, 2=>idle, 3=>dnd}).
-define(CHANNEL_TYPE_MAP, #{0=>normal, 1=>wall}).
-define(ENTITY_STRUCTURE, #{
    user => #{
            id              => {0,  number,   {8}},
            email           => {1,  string,   {}},
            name            => {2,  string,   {}},
            tag             => {3,  number,   {3}},
            status          => {4,  atom,     {1, ?USER_STATUS_MAP}},
            status_text     => {5,  string,   {}},
            ava_file        => {7,  number,   {8}},
            mfa_enabled     => {8,  bool,     {}},
            friends         => {9,  num_list, {8}},
            blocked         => {10, num_list, {8}},
            pending_in      => {11, num_list, {8}},
            pending_out     => {12, num_list, {8}},
            dm_channels     => {13, num_list, {8}},
            groups          => {14, num_list, {8}},
            roles           => {15, num_list, {8}},
            color           => {16, number,   {3}},
            badges          => {17, num_list, {1}},
            bot_owner       => {18, number,   {8}},
            owned_bots      => {19, num_list, {8}},
            wall            => {20, number,   {8}},
            email_confirmed => {21, bool,     {}}
        },
    channel => #{
            id           => {0, number,   {8}},
            name         => {1, string,   {}},
            members      => {2, num_list, {8}},
            group        => {3, number,   {8}},
            messages     => {4, num_list, {8}},
            typing       => {5, num_list, {8}},
            type         => {6, atom,     {1, ?CHANNEL_TYPE_MAP}},
            unread       => {7, number,   {4}},
            first_unread => {8, number,   {8}}
        },
    message => #{
            id      => {0, number,   {8}},
            states  => {1, num_list, {8}},
            channel => {2, number,   {8}},
            sender  => {3, number,   {8}}
        },
    file => #{
            id         => {0, number, {8}},
            name       => {1, string, {}},
            pixel_size => {2, string, {}},
            preview    => {3, string, {}},
            length     => {4, number, {4}}
        },
    message_state => #{
            id       => {0, number, {8}},
            msg_id   => {1, number, {8}},
            sections => {2, list,   {1, fun datatypes:enc_msg_section/1,
                                        fun datatypes:len_dec_msg_section/1}}
        }
}).