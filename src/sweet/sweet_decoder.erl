%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(sweet_decoder).
-author("Yamka").
-license("MPL-2.0").
-description("Packet decoder process for each client").

-include("../packets/packet.hrl").

-export([start/3]).

start(Main, Socket, Proto) ->
    loop(Main, Socket, Proto).

loop(Main, Socket, Proto) ->
    % read packet
    <<Data/bitstring>> = read_packet_data(Socket),
    Packet = decode_packet_data(Data, Proto),

    % send it to the main process
    Main ! {packet, self(), Packet},

    % repeat
    loop(Main, Socket, Proto).

read_packet_data(Socket) ->
    % read the compression header
    {ok, CHdrList} = ssl:recv(Socket, 4),
    <<Compressed:8, CompressedLen:24>> = list_to_binary(CHdrList),

    % read the (possibly compressed) data header and payload
    {ok, CDataList} = ssl:recv(Socket, CompressedLen),
    CData = list_to_binary(CDataList),
    if
        Compressed > 0 -> zlib:gunzip(CData);
        true -> CData
    end.

decode_packet_data(Data, ProtoVersion) ->
    % deconstruct binary
    <<TypeCode:8,
      Seq:32,
      Reply:32,
      CapLen:16, Captcha:CapLen/bytes-unit:8,
      Payload/bytes>> = Data,

    % convert numeric packet type to an atom
    #{TypeCode := Type} = ?PACKET_TYPE_MAP,

    % call the decoding function
    Fields = (list_to_existing_atom(atom_to_list(Type) ++ "_packet")):decode(Payload, ProtoVersion),

    % return the result
    #packet{
        type = Type,
        seq = Seq,
        reply = Reply,
        captcha = Captcha,
        fields = Fields
    }.