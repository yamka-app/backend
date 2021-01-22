-module(file_download_request_packet).
-author("Order").
-license("MPL-2.0").

-export([decode/2]).

decode(<<Id:64/unsigned-integer>>, Proto) when Proto >= 5 -> #{id => Id}.