%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(sweet_encoder).
-author("Yamka").
-license("MPL-2.0").
-description("Packet encoder process for each client").

-include("../packets/packet.hrl").
-define(COMPRESSION_THRESHOLD, 128). % used when sending only

-export([start/3]).

start(Main, Socket, SetupData) ->
    loop(Main, Socket, SetupData).

loop(Main, Socket, SetupData={Proto, Comp}) ->
    receive
        {packet, Main, Packet} ->
            Data = encode_packet_data(Packet, SetupData),
            write_packet_data(Socket, Data, Comp)
    end,
    loop(Main, Socket, SetupData).

encode_packet_data(Packet, Proto) ->
    #packet{type=Type, seq=Seq, reply=Reply, fields=Fields} = Packet,

    % encode payload
    Payload = (list_to_existing_atom(atom_to_list(Type) ++ "_packet")):encode(Fields, Proto),

    % get numeric type
    #{Type := TypeCode} = ?REVERSE_PACKET_TYPE_MAP,
    
    % encode data
    <<TypeCode:8,
      Seq:32,
      Reply:32,
      Payload/binary>>.

write_packet_data(Socket, Data, Comp) ->
    % compress the data
    ShouldCompress = Comp andalso (byte_size(Data) >= ?COMPRESSION_THRESHOLD),
    Compressed = if
        ShouldCompress -> zlib:gzip(Data);
        true -> Data
    end,

    % add a compression header
    ToSend = <<(if ShouldCompress->1;true->0 end):8, (byte_size(Compressed)):24, Compressed/binary>>,

    % send data
    ssl:send(Socket, ToSend).