-module(calc_server).
-export([start/0]).

start() -> spawn_link(fun init/0).

init() ->
	{ok, Socket} = gen_udp:open(8889, [binary, {active, true}]), 
	loop(Socket).

loop(Socket) ->
	receive
		{udp, Socket, Host, Port, Bin} ->
			Pid = self(),
			spawn(fun() -> calc(Pid, {Socket, Host, Port}, Bin) end);
		{calc, {Socket, Host, Port}, Bin} ->
			spawn(fun() -> ok = gen_udp:send(Socket, Host, Port, Bin) end)
			
	end,
	loop(Socket).

calc(Pid, Data, Bin) ->
	Str = binary_to_list(Bin),
	Res = rpn:calc(Str),
	BinRes = convert(Res),
	Pid ! {calc, Data, BinRes}.

convert(N) ->
	List = convert0(N),
	Bin = list_to_binary(List),
	Bin.

convert0(N) when is_float(N) ->
	List = float_to_list(N),
	List;
convert0(N) when is_integer(N) ->
	List = integer_to_list(N),
	List.