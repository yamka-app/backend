%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(sweet_decoder).
-author("Yamka").
-license("MPL-2.0").
-description("Packet decoder process for each client").

-include("../packets/packet.hrl").

-export([start_link/2, loop/2]).

start_link(Main, Conn) -> {ok, spawn_link(?MODULE, loop, [Main, Conn])}.

loop(Main, Conn={Socket, Proto}) ->
    % read packet
    <<Data/bitstring>> = read_packet_data(Socket),
    Packet = decode_packet_data(Data, Proto),

    % send it to the main process
    sweet_main:packet(Main, Packet),

    % repeat
    loop(Main, Conn).

read_packet_data(Socket) ->
    % read the compression header
    {ok, <<Compressed:8, CompressedLen:24>>} = ssl:recv(Socket, 4),

    % read the (possibly compressed) data header and payload
    {ok, CData} = ssl:recv(Socket, CompressedLen),
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