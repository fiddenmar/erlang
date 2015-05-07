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

calc(L) when is_list(L) ->
	[Res] = lists:foldl(fun calc/2, [], string:tokens(L, " ")),
	Res.

calc("+", [N1, N2|S]) -> [N2+N1|S];
calc("*", [N1, N2|S]) -> [N2*N1|S];
calc("-", [N1, N2|S]) -> [N2-N1|S];
calc("/", [N1, N2|S]) -> [N2/N1|S];
calc("^", [N1, N2|S]) -> [math:pow(N2,N1)|S];
calc(X, Stack) -> [read(X)|Stack].

read(N) -> 
	case string:to_float(N) of
		{error, no_float} -> list_to_integer(N);
		{F, _} -> F
	end.