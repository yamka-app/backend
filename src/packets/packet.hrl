-record(packet, { type=unknown, seq=0, reply=0, captcha="", fields=#{} }).

-define(PACKET_TYPE_MAP, #{
    1  => login,
    2  => ping,
    3  => pong,
    5  => signup,
    18 => identification
}).

-define(REVERSE_PACKET_TYPE_MAP, #{
    ping            => 2,
    pong            => 3,
    status          => 4,
    client_identity => 19
}).

-define(STATUS_CODE_MAP, #{
    unsupported_proto             => 1,
    invalid_connection_state      => 2,
    login_error                   => 3,
    mfa_required                  => 4,
    signup_error                  => 6,
    rate_limiting                 => 7,
    invalid_id                    => 8,
    file_too_large                => 9,
    permission_denied             => 10,
    invalid_cont_token            => 11,
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