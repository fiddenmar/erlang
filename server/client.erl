-module(client).
-export([start/1]).

start(N) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    BinStr = list_to_binary(N),
    ok = gen_udp:send(Socket, "localhost", 8889, BinStr),
    Value = receive
                {udp, Socket, _, _, Bin} ->
                    Bin
            end,
    gen_udp:close(Socket),
    Str = binary_to_list(Value),
    Str.