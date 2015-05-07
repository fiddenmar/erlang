-module(rpn).
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

