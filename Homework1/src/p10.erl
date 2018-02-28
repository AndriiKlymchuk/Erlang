-module(p10).

-export([encode/1]).

-spec(encode(L :: list()) ->
	[{pos_integer(), term()}]).
encode([H|T]) ->
	p05:reverse(encode(T, {1, H}, []));
encode([]) ->
	[].

-spec(encode(L :: list(), Tup :: tuple(), Acc :: list()) ->
	[{pos_integer(), term()}]).
encode([H|T], {N, H}, Acc) ->
	encode(T, {N + 1, H}, Acc);
encode([H|T], Tup, Acc) ->
	encode(T, {1, H}, [Tup|Acc]);
encode([], Tup, Acc) ->
	[Tup|Acc].