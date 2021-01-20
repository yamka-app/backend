-record(packet, {type=unknown, seq=0, reply=0, captcha="", fields=#{}}).

-define(REVERSE_PACKET_TYPE_MAP, utils:swap_map(?PACKET_TYPE_MAP)).
-define(PACKET_TYPE_MAP, #{
    1  => login,
    2  => ping,
    3  => pong,
    4  => status,
    5  => signup,
    6  => entity_get,
    7  => entities,
    12 => access_token,
    18 => identification,
    19 => client_identity
}).

-define(STATUS_CODE_MAP, #{
    unsupported_proto             => 1,
    invalid_connection_state      => 2,
    login_error                   => 3,
    mfa_required                  => 4,
    signup_success                => 5,
    signup_error                  => 6,
    rate_limiting                 => 7,
    invalid_id                    => 8,
    file_too_large                => 9,
    permission_denied             => 10,
    invalid_access_token          => 11,
    user_not_pending              => 12,
    contact_action_not_applicable => 13,
    invalid_username              => 14,
    invalid_entity                => 15,
    entity_not_paginable          => 16,
    invalid_invite                => 17,
    internal_error                => 18,
    unknown_packet                => 19,
    friend_request_sent           => 20,
    packet_parsing_error          => 21
}).

-define(REVERSE_TOKEN_PERMISSION_MAP, utils:swap_map(?TOKEN_PERMISSION_MAP)).
-define(ALL_PERMISSIONS_EXCEPT_BOT, [maps:get(C, ?TOKEN_PERMISSION_MAP) || C <- lists:seq(0, 25)]).
-define(TOKEN_PERMISSION_MAP, #{
    0  => see_profile,
    1  => see_relationships, % friends, blocked users, etc.
    2  => see_groups,
    3  => see_direct_messages,
    4  => edit_profile,
    5  => edit_relationships,

    6  => send_group_messages,
    7  => send_direct_messages,
    8  => receive_group_messages,
    9  => read_group_message_history,
    10 => receive_direct_messages,
    11 => read_direct_message_history,
    12 => delete_group_messages,
    13 => delete_direct_messages,

    14 => create_groups,
    15 => edit_groups,
    16 => delete_groups,
    17 => join_groups,
    18 => leave_groups,
    19 => ban_members,
    20 => kick_members,
    21 => manage_roles,
    22 => delete_others_messages,

    23 => read_own_wall,
    24 => read_others_walls,
    25 => post_on_own_wall,

    26 => bot
}).