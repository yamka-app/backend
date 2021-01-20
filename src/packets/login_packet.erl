-module(login_packet).
-author("Order").
-license("MPL-2.0").

-include("packet.hrl").

-export([decode/2]).

decode(Payload, ProtocolVersion) when ProtocolVersion >= 5 ->
    Email    = datatypes:dec_str(Payload),
    EmailLen = datatypes:len_str(Payload),

    PasswordBin = binary:part(Payload, EmailLen, byte_size(Payload) - EmailLen),
    Password    = datatypes:dec_str(PasswordBin),
    PasswordLen = datatypes:len_str(PasswordBin),

    PermsBin = binary:part(Payload, EmailLen+PasswordLen, byte_size(Payload)-EmailLen-PasswordLen),
    PermsNum = datatypes:dec_num_list(PermsBin, 1),
    Perms = [maps:get(C, ?TOKEN_PERMISSION_MAP) || C <- PermsNum],

    #{email => Email, password => Password, perms => Perms}.