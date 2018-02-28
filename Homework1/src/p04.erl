-module(p04).

-export([len/1]).

-spec(len(L :: list()) ->
	non_neg_integer()).
len(L) ->
	len(L, 0).

-spec(len(L :: list(), Acc :: non_neg_integer()) ->
	non_neg_integer()).
len([_|T], Acc) ->
	len(T, Acc + 1);
len([], Acc) ->
	Acc.
	