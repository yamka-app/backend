-module(file_client).
-author("Order").
-license("MPL-2.0").
-description("File process. Sends and receives files. Depends on a \"normal client\" process. In fact, it's using").

-define(CHUNK_SIZE, 1024*10). % bytes
-include("packets/packet.hrl").
-include_lib("cqerl/include/cqerl.hrl").

-export([client_init/2]).

send_chunk(Handle, Position, Reply) ->
    {ok, Data} = file:read(Handle, ?CHUNK_SIZE),
    send_packet(file_data_chunk_packet:make(Position, Data, Reply)),
    case byte_size(Data) of
        ?CHUNK_SIZE -> send_chunk(Handle, Position + byte_size(Data), Reply);
        _ -> send_packet(status_packet:make(stream_end, "File transfer finished", Reply))
    end.

client_init(Settings, Task) ->
    {Socket, Protocol} = Settings,
    put(socket, Socket), put(protocol, Protocol),
    case Task of
        {send_file, Path, Reply} ->
            {ok, Handle} = file:open(Path, [read, binary]),
            send_chunk(Handle, 0, Reply)
    end,
    exit(normal).

send_packet(P) ->
    logging:log("<--F ~p", [packet_iface:clear_for_printing(P)]),
    spawn_monitor(packet_iface, writer, [
        get(socket), P,
        get(protocol), false, self()
    ]),
    receive
        _ -> ok
    end.