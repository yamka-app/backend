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
    8  => file_download_request,
    9  => file_data_chunk,
    10 => mfa_secret,
    12 => access_token,
    13 => contacts_manage,
    14 => user_search,
    15 => invite_resolve,
    16 => bot_create,
    17 => bot_invite,
    18 => identification,
    19 => client_identity
}).

-define(REVERSE_STATUS_CODE_MAP, utils:swap_map(?STATUS_CODE_MAP)).
-define(STATUS_CODE_MAP, #{
    1  => unsupported_proto,
    2  => invalid_connection_state,
    3  => login_error,
    4  => mfa_required,
    5  => signup_success,
    6  => signup_error,
    7  => rate_limiting,
    8  => invalid_id,
    9  => file_too_large,
    10 => permission_denied,
    11 => invalid_access_token,
    12 => user_not_pending,
    13 => contact_action_not_applicable,
    14 => invalid_username,
    15 => invalid_entity,
    16 => entity_not_paginable,
    17 => invalid_invite,
    18 => internal_error,
    19 => unknown_packet,
    20 => friend_request_sent,
    21 => packet_parsing_error,
    22 => start_uploading,
    23 => stream_end
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