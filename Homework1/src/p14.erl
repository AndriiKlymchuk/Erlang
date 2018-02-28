-module(p14).

-export([duplicate/1]).

-spec(duplicate(L :: list()) ->
	list()).
duplicate(L) ->
	p05:reverse(duplicate(L, [])).

-spec(duplicate(L :: list(), Acc :: list()) ->
	list()).
duplicate([H|T], Acc) ->
	duplicate(T, [H,H|Acc]);
duplicate([], Acc) ->
	Acc.