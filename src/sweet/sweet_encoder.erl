%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(sweet_encoder).
-author("Yamka").
-license("MPL-2.0").
-description("Packet encoder process for each client").

-include("../packets/packet.hrl").

-export([start_link/2, init/2]).

start_link(Main, Conn={_, _}) -> {ok, spawn_link(?MODULE, init, [Main, Conn])}.

init(Main, Conn={_, _}) -> Main ! {encoder_pid, self()}, loop(Main, Conn).
loop(Main, Conn={Socket, Proto}) ->
    receive
        {packet, Packet} ->
            Data = encode_packet_data(Packet, Proto),
            write_packet_data(Socket, Data)
    end,
    loop(Main, Conn).

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

write_packet_data(Socket, Data) ->
    % compress the data
    ShouldCompress = byte_size(Data) >= yamka_config:get(sweet_comp_threshold),
    Compressed = if
        ShouldCompress -> zlib:gzip(Data);
        true -> Data
    end,

    % add a compression header
    ToSend = <<(if ShouldCompress->1;true->0 end):8, (byte_size(Compressed)):24, Compressed/binary>>,

    % send data
    ssl:send(Socket, ToSend).