-module(tasty_client).
-author("Order").
-license("MPL-2.0").
-description("The Tasty (voice protocol) UDP listener").

-define(TIMEOUT, 15000).

-export([handle_packet/2, handler/2, controller/2]).

handle_packet(Src, Packet) -> spawn(?MODULE, handler, [Src, Packet]).

%% "identitification" packet
handler(Src={IP, Port}, <<0:8/unsigned-integer, SId:128/unsigned-integer>>) ->
    Session = tasty:get_session(SId),
    tasty:register_user(SId, IP, Port, spawn_link(?MODULE, controller, [Session, Src]));

%% normal packet
handler(Src={_,_}, <<1:8/unsigned-integer, Encrypted/binary>>) ->
    {_, Pub, _, _, _, Controller} = tasty:get_session(Src),
    Controller ! {packet, crypto:public_decrypt(rsa, Pub, Encrypted, [])};

%% voice data
handler({_, _, _, _, Chan, _}, <<0:8/unsigned-integer, Data/binary>>) ->
    tasty:broadcast(Chan, Data).

controller(Session={SId, _, Priv, _, _, _}, Src={_,_}) ->
    receive
        {packet, P} ->
            case handler(Session, P) of
                {send, P} ->
                    Encrypted = crypto:private_encrypt(rsa, Priv, P, []),
                    tasty_listener ! {send, Src, Encrypted}
            end;
        {broadcast, P} ->
            Encrypted = crypto:private_encrypt(rsa, Priv, P, []),
            tasty_listener ! {send, Src, Encrypted}
    after ?TIMEOUT ->
        tasty:unregister_user(SId)
    end,
    controller(Session, Src).