%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(tasty_client).
-author("Yamka").
-license("MPL-2.0").
-description("The Tasty (voice/video protocol) UDP listener").

-define(TIMEOUT, 15000).
-define(PACKET_RATE_LIMIT, 100).
-define(PACKET_SIZE_LIMIT, 128).
-define(SPEAKING_IND_THRESHOLD, 250).

-export([handle_packet/2, handler/2, controller_init/2]).
-export([speaking_status_timeout/1]).

%%% encryption helpers
%% encrypt data
enc_chunk(<<Data/binary>>, <<Key:128/bitstring, IV:128/bitstring>>) ->
    crypto:crypto_one_time(aes_128_cfb128, Key, IV, Data, true).
%% decrypt data
dec_chunk(<<Data/binary>>, <<Key:128/bitstring, IV:128/bitstring>>) ->
    crypto:crypto_one_time(aes_128_cfb128, Key, IV, Data, false).

handle_packet(Src, Packet) -> spawn(?MODULE, handler, [Src, Packet]).

%% "identitification" packet
handler(Src={_,_}, <<0, SId:128/bitstring>>) ->
    Session = tasty:get_session(SId),
    tasty:register_user(SId, Src, spawn(?MODULE, controller_init, [Session, Src]));

%% normal packet
handler(Src={_,_}, <<1, Encrypted/binary>>) ->
    {_, Key, _, _, Controller} = tasty:get_session(Src),
    Controller ! {packet, dec_chunk(Encrypted, Key)}.

%% voice data
dec_handler({_, _, User, Chan, _}, <<0, Data/binary>>) ->
    Allow = check_data_lims(Data),
    if Allow ->
            tasty:broadcast(Chan, Data, User);
       true -> drop
    end;

%% disconnect notice
dec_handler(_, <<1>>) -> drop;

%% heartbeat
dec_handler(_, <<2>>) -> ok.

%% checks voice data packet limits
check_data_lims(Data) ->
    true.
    % (byte_size(Data) =< ?PACKET_SIZE_LIMIT) and
    % (ratelimit:hit(packet, 1) == 1).

%%% the controller is responsible for receiving, decypting and parsing
%%% data packets from one specific client, asking the local Tasty gen_server
%%% to broadcast them if needed and accepting and fulfilling broadcast
%%% request from the aforementioned server

%% controller init function
controller_init(Session={Id, Key, _, _, _}, Src) ->
    ratelimit:make(packet, {?PACKET_RATE_LIMIT, 1000}),
    tasty_listener ! {send, Src, enc_chunk(<<0>>, Key)},
    Timeout = spawn_link(?MODULE, speaking_status_timeout, [Id]),
    controller_loop(Session, Src, Timeout).

%% controller loop
controller_loop(Session={SId, Key, _, _, _}, Src={_,_}, Timeout) ->
    receive
        % client packet
        {packet, P} ->
            Timeout ! packet,
            case dec_handler(Session, P) of
                % this may be useful but dialyzer is not happy:
                % {packet, Data} ->
                %     Encrypted = enc_chunk(Data, Key),
                %     tasty_listener ! {send, Src, Encrypted};
                drop ->
                    tasty:unregister_user(SId),
                    exit(normal);
                _ -> ok
            end;
        % voice/video data from other client
        {broadcast, P} ->
            tasty_listener ! {send, Src, enc_chunk(P, Key)}
    after ?TIMEOUT ->
        tasty:unregister_user(SId),
        exit(normal)
    end,
    controller_loop(Session, Src, Timeout).

speaking_status_timeout(Session) ->
    receive
        packet ->
            % repeating requests are ignored
            tasty:add_speaking_flag(Session)
    after ?SPEAKING_IND_THRESHOLD ->
        tasty:rm_speaking_flag(Session)
    end,
    speaking_status_timeout(Session).