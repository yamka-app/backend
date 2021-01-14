-module(login_packet).
-author("Order").
-license("MPL-2.0").

-export([decode/2]).

decode(Payload, ProtocolVersion) when ProtocolVersion >= 5 ->
    Email    = datatypes:dec_str(Payload),
    EmailLen = datatypes:len_str(Payload),
    Password = datatypes:dec_str(binary:part(Payload, EmailLen, byte_size(Payload) - EmailLen)),
    #{ email => Email, password => Password }.