-module(login_packet).
-author("Order").
-license("MPL-2.0").

-export([decode/1]).

decode(Payload) ->
    Login    = datatypes:dec_str(Payload),
    LoginLen = datatypes:len_str(Payload),
    Password = datatypes:dec_str(binary:part(Payload, LoginLen, byte_size(Payload) - LoginLen)),
    #{ login => Login, password => Password }.