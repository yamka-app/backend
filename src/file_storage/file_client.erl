%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(file_client).
-author("Yamka").
-license("MPL-2.0").
-description("File process. Sends and receives files. Depends on a \"normal client\" process").

-include("../packets/packet.hrl").
-include("../entities/entity.hrl").
-include_lib("cqerl/include/cqerl.hrl").

-export([client_init/2]).

recv_chunk(Handle, Length) ->
    receive
        <<DataChunk/binary>> ->
            Trimmed = binary:part(DataChunk, 0, min(Length, byte_size(DataChunk))),
            ChunkLen = byte_size(Trimmed),
            lager:debug("client file transfer: ~p/~p", [ChunkLen, Length]),
            file:write(Handle, Trimmed),
            if
                ChunkLen >= Length -> ok;
                true -> recv_chunk(Handle, Length - ChunkLen)
            end;

        _ -> recv_chunk(Handle, Length)
    end.

send_chunk(Handle, Position, Reply) ->
    ChunkSize = yamka_config:get(sweet_file_chunk_size),
    {ok, Data} = file:read(Handle, ChunkSize),
    sweet_main:send_packet(get(main), file_data_chunk_packet:make(Position, Data, Reply)),
    case byte_size(Data) of
        ChunkSize -> send_chunk(Handle, Position + byte_size(Data), Reply);
        _ -> sweet_main:send_packet(get(main), status_packet:make(stream_end, "File transfer finished", Reply))
    end.

%% sends a file
client_init(Main, {send_file, Path, Reply}) ->
    put(main, Main),

    {ok, Handle} = file:open(Path, [read, binary]),
    send_chunk(Handle, 0, Reply),

    exit(normal);

%% receives a file
client_init({Socket, Protocol, Host, Cassandra}, {recv_file, Length, Name, EmojiName, Reply}) ->
    put(socket, Socket), put(protocol, Protocol), put(cassandra, Cassandra),

    sweet_main:send_packet(get(main), status_packet:make(start_uploading, "Start uploading", Reply)),

    Path = utils:temp_file_name(),
    {ok, Handle} = file:open(Path, [write, binary]),
    recv_chunk(Handle, Length),
    Id = file_storage:register_file(Path, Name, EmojiName),
    sweet_main:send_packet(get(main), entities_packet:make([#entity{type=file, fields=file_e:get(Id)}], Reply)),

    Host ! upload_fin,
    exit(normal).