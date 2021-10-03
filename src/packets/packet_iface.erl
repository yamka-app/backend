%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(packet_iface).
-author("Yamka").
-license("MPL-2.0").
-description("Packet encoding and decoding").

-include("packet.hrl").

-export([reader/3, writer/5, clear_for_printing/1]).

%% decodes a packet
decode(Data, Proto) ->
    TypeCode = datatypes:dec_num(binary:part(Data, 0, 1)),
    Seq      = datatypes:dec_num(binary:part(Data, 1, 4)),
    Reply    = datatypes:dec_num(binary:part(Data, 5, 4)),
    Captcha  = datatypes:dec_str(binary:part(Data, 9, byte_size(Data) - 9)),
    CapLen   = datatypes:len_str(binary:part(Data, 9, byte_size(Data) - 9)),
    Payload  = binary:part(Data, 9 + CapLen, byte_size(Data) - 9 - CapLen),

    Type = try maps:get(TypeCode, ?PACKET_TYPE_MAP) of
        Val -> Val
    catch
        _:_ -> unknown
    end,

    %% try to parse the packet payload
    %% evaluate to {ok, #packet} in case it succeeded,
    %% evaluate to {error, Seq} in case it failed
    try case Type of
            login                 -> fun login_packet                :decode/2;
            ping                  -> fun ping_packet                 :decode/2;
            identification        -> fun identification_packet       :decode/2;
            signup                -> fun signup_packet               :decode/2;
            access_token          -> fun access_token_packet         :decode/2;
            contacts_manage       -> fun contacts_manage_packet      :decode/2;
            entity_get            -> fun entity_get_packet           :decode/2;
            entities              -> fun entities_packet             :decode/2;
            file_download_request -> fun file_download_request_packet:decode/2;
            file_data_chunk       -> fun file_data_chunk_packet      :decode/2;
            mfa_secret            -> fun mfa_secret_packet           :decode/2;
            search                -> fun search_packet               :decode/2;
            invite_resolve        -> fun invite_resolve_packet       :decode/2;
            voice_join            -> fun voice_join_packet           :decode/2;
            email_confirmation    -> fun email_confirmation_packet   :decode/2;
            password_change       -> fun password_change_packet      :decode/2;
            mfa_toggle            -> fun mfa_toggle_packet           :decode/2;
            _                     -> throw(unknown)
        end
    of
        F -> {ok, #packet{type    = Type,
                          seq     = Seq,
                          reply   = Reply,
                          captcha = Captcha,
                          fields  = F(Payload, Proto)}}
    catch
        throw:unknown -> {error, Seq, Type, {unknown_packet}};
        E:D:T -> {error, Seq, Type, {E, D, T}}
    end.

%% encodes a packet
encode(Packet, Proto) ->
    Fields = Packet#packet.fields,
    Payload = case Packet#packet.type of
        status          -> fun status_packet         :encode/2;
        client_identity -> fun client_identity_packet:encode/2;
        pong            -> fun pong_packet           :encode/2;
        access_token    -> fun access_token_packet   :encode/2;
        entities        -> fun entities_packet       :encode/2;
        file_data_chunk -> fun file_data_chunk_packet:encode/2;
        mfa_secret      -> fun mfa_secret_packet     :encode/2;
        voice_join      -> fun voice_join_packet     :encode/2;
        search_result   -> fun search_result_packet  :encode/2
    end(Fields, Proto),
    
    Type     = datatypes:enc_num(maps:get(Packet#packet.type, ?REVERSE_PACKET_TYPE_MAP), 1),
    SeqBin   = datatypes:enc_num(Packet#packet.seq, 4),
    ReplyBin = datatypes:enc_num(Packet#packet.reply, 4),

    <<Type/binary, SeqBin/binary, ReplyBin/binary, Payload/binary>>.

%% reads a packet complete with decompression logic
reader(Socket, Protocol, Pid) ->
    % read the compression header
    {ok, CHdrList} = ssl:recv(Socket, 4),
    CHdr = list_to_binary(CHdrList),
    Compressed    = datatypes:dec_bool(binary:part(CHdr, 0, 1)),
    CompressedLen = datatypes:dec_num (binary:part(CHdr, 1, 3)),

    % read the (possibly compressed) data header and payload
    {ok, CDataList} = ssl:recv(Socket, CompressedLen),
    CData = list_to_binary(CDataList),
    Data = if
        Compressed -> zlib:gunzip(CData);
        true -> CData
    end,

    % decode data and send it
    Pid ! case decode(Data, Protocol) of
        {ok, P}          -> {packet, P};
        {error, S, T, E} -> {decoding_error, S, T, E}
    end.

%% writes a packet coomplete with compression logic
writer(Socket, Packet, Proto, SupportsCompression, Pid) ->
    %% encode data
    Data = encode(Packet, Proto),

    %% compress it if necessary
    Compressed = (byte_size(Data) >= application:get_env(yamkabackend, sweet_comp_threshold)) and SupportsCompression,
    CData = if
        Compressed -> zlib:gzip(Data);
        true -> Data
    end,

    %% add the compression header
    CBin = datatypes:enc_bool(Compressed),
    CLBin = datatypes:enc_num(byte_size(CData), 3),
    ssl:send(Socket, <<CBin/binary, CLBin/binary, CData/binary>>),
    
    Pid ! {sent, Packet#packet.seq}.

%% clears out "spammy" unneeded fields for nice logging output
clear_for_printing(#packet{type=T, fields=F}=P) -> P#packet{fields=clear_fields_for_printing(T, F)}.

clear_fields_for_printing(file_data_chunk, F) -> maps:put(data, removed, F);
clear_fields_for_printing(_, F) -> F.