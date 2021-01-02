-record(packet, { type, seq, reply, fields }).

-define(PACKET_TYPE_MAP, #{
    1 => login,
    2 => ping,
    3 => pong
}).