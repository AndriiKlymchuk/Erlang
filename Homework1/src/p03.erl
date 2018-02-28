-module(p03).

-export([element_at/2]).

-spec(element_at(L :: list(), N :: pos_integer()) ->
	term()).
element_at([H|_], 1) ->
	H;
element_at([_|T], N) ->
	element_at(T, N - 1);
element_at([], _) ->
	undefined.
	