-module(p08).

-export([compress/1]).

-spec(compress(L :: list()) ->
	list()).
compress(L) ->
	p05:reverse(compress(L, [])).

-spec(compress(L :: list(), Acc :: list()) ->
	list()).
compress([H|T], Acc = [H|_]) ->
	compress(T, Acc);
compress([H|T], Acc) ->
	compress(T, [H|Acc]);
compress([], Acc) ->
	Acc.