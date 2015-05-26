%%calc_rpn module
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

-module(calc_rpn).
-export([calc/1]).

fold(_, Start, []) -> Start;
fold(F, {ignore, Start}, [H|T]) -> fold(F, F(H,Start,ignore), T);
fold(F, Start, [H|T]) -> fold(F, F(H,Start,T), T).

take_first([H|_]) -> H.

replace(Changes, Str) -> 
	lists:map(fun(Key) -> case lists:keyfind(Key, 1, Changes) of {Key, Result} -> Result; false -> Key end end, Str).

calc(L) when is_list(L) ->
	[Res] = fold(fun calc/3, [], string:tokens(L, " ")),
	Res.

calc("endfun", S, ignore) -> S;
calc(_, S, ignore) -> {ignore, S};
calc("endargs", S, _) -> S;
calc("fun", S, T) ->
	ArgsKey = lists:takewhile(fun(E) -> E =/= "endargs" end, T),
	{ArgsValue, NewS} = lists:split(lists:flatlength(ArgsKey), S),
	ArgsValueStr = lists:map(fun(E) -> convert(E) end, ArgsValue),
	ArgsTuple = lists:zip(ArgsKey, ArgsValueStr),
	Fun = lists:takewhile(fun(E) -> E =/= "endfun" end, lists:dropwhile(fun(E) -> E =/= "endargs" end, T)),
	St = replace(ArgsTuple, Fun),
	{ignore, [take_first(fold(fun calc/3, [], St))|NewS]};
calc("+", [N1, N2|S], _) -> [N2+N1|S];
calc("*", [N1, N2|S], _) -> [N2*N1|S];
calc("-", [N1, N2|S], _) -> [N2-N1|S];
calc("/", [N1, N2|S], _) -> [N2/N1|S];
calc("^", [N1, N2|S], _) -> [math:pow(N2,N1)|S];
calc("ln", [N|S], _) -> [math:log(N)|S];
calc("log10", [N|S], _) -> [math:log10(N)|S];
calc("sum", S, _) -> [lists:sum(S)];
calc("prod", S, _) -> [lists:foldl(fun erlang:'*'/2, 1, S)];
calc("sin", [N|S], _) -> [math:sin(N)|S];
calc("cos", [N|S], _) -> [math:cos(N)|S];
calc(X, Stack, _) -> [read(X)|Stack].

read(N) -> 
	case string:to_float(N) of
		{error, no_float} -> list_to_integer(N);
		{F, _} -> F
	end.

convert(N) when is_float(N) ->
	List = float_to_list(N),
	List;
convert(N) when is_integer(N) ->
	List = integer_to_list(N),
	List.