-module(p05).

-export([reverse/1]).

-spec(reverse(L :: list()) ->
	list()).
reverse(L) ->
	reverse(L, []).

-spec(reverse(L :: list(), Acc :: list()) ->
	list()).
reverse([H|T], Acc) ->
	reverse(T, [H|Acc]);
reverse([], Acc) ->
	Acc.