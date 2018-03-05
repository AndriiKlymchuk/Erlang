-module(p15).

-export([replicate/2]).

%% Infinite loop possible if N less than 1
%% Need to add guard when N >= 1

-spec(replicate(L :: list(), N :: pos_integer()) ->
	list()).
replicate(L, N) ->
	p05:reverse(replicate(L, N, N, [])).

-spec(replicate(L 	  :: list(),
				InitN :: pos_integer(),
				N     :: pos_integer(),
				Acc	  :: list()) ->
	list()).
replicate([H|T], InitN, 1, Acc) ->
	replicate(T, InitN, InitN, [H|Acc]);
replicate(L = [H|_], InitN, N, Acc) ->
	replicate(L, InitN, N - 1, [H|Acc]);
replicate([], _, _, Acc) ->
	Acc.
