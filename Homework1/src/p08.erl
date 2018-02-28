-module(p08).

-export([compress/1]).

-spec(compress(L :: list()) ->
	list()).
compress([H|T]) ->
	p05:reverse(compress(T, [H]));
compress([]) ->
	[].

-spec(compress(L :: list(), Acc :: list()) ->
	list()).
compress([H|T], Acc = [H|_]) ->
	compress(T, Acc);
compress([H|T], Acc) ->
	compress(T, [H|Acc]);
compress([], Acc) ->
	Acc.