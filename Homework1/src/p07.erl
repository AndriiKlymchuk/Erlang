-module(p07).

-export([flatten/1]).

-spec(flatten(L :: list()) ->
	list()).
flatten(L) ->
	flatten(L, []).

-spec(flatten(L :: list(), Acc :: list()) ->
	list()).
flatten([H|T], Acc) ->
	flatten(H, flatten(T, Acc));
flatten([], Acc) ->
	Acc;
flatten(El, Acc) ->
	[El|Acc].