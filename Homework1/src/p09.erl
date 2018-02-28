-module(p09).

-export([pack/1]).

-spec(pack(L :: list()) ->
	list()).
pack([H|T]) ->
	p05:reverse(pack(T, [H], []));
pack([]) ->
	[].

-spec(pack(L :: list(), SubL :: list(), Acc :: list()) ->
	list()).
pack([H|T], SubL = [H|_], Acc) ->
	pack(T, [H|SubL], Acc);
pack([H|T], SubL, Acc) ->
	pack(T, [H], [SubL|Acc]);
pack([], SubL, Acc) ->
	[SubL|Acc].
