%%calc_server module
%%
%%The MIT License (MIT)
%%
%%Copyright (c) 2015 Evgeny Petrov, fiddenmar@gmail.com
%%
%%Permission is hereby granted, free of charge, to any person obtaining a copy
%%of this software and associated documentation files (the "Software"), to deal
%%in the Software without restriction, including without limitation the rights
%%to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%copies of the Software, and to permit persons to whom the Software is
%%furnished to do so, subject to the following conditions:
%%
%%The above copyright notice and this permission notice shall be included in all
%%copies or substantial portions of the Software.
%%
%%THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%%SOFTWARE.

-module(calc_server).
-export([start/0]).

start() -> spawn_link(fun init/0).

init() ->
	{ok, Socket} = gen_udp:open(8889, [binary, {active, true}]), 
	loop(Socket).

loop(Socket) ->
	Pid = self(),
	loop(Socket, Pid).

loop(Socket, Pid) ->
	receive
		{udp, Socket, Host, Port, Bin} ->
			spawn(fun() -> calc(Pid, {Socket, Host, Port}, Bin) end);
		{calc, {Socket, Host, Port}, Bin} ->
			spawn(fun() -> ok = gen_udp:send(Socket, Host, Port, Bin) end)
			
	end,
	loop(Socket, Pid).

calc(Pid, Data, Bin) ->
	Str = binary_to_list(Bin),
	Res = calc_rpn:calc(Str),
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