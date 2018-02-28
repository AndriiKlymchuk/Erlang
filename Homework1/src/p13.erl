-module(p13).

-export([decode/1]).

-spec(decode(List :: [{pos_integer(), term()}]) ->
	list()).
decode([H|T]) ->
	p05:reverse(decode(T, H, []));
decode([]) ->
	[].

%% Infinite loop possible if number in tuple less than 1
%% Need to add guard when N > 1 to third clause

-spec(decode(L   :: [{pos_integer(), term()}],
			 El  :: {pos_integer(), term()},
			 Acc :: list()) ->
	list()).

decode([H|T], {1, Term}, Acc) ->
	decode(T, H, [Term|Acc]);
decode([], {1, Term}, Acc) ->
	[Term|Acc];
decode(L, {N, Term}, Acc) ->
	decode(L, {N - 1, Term}, [Term|Acc]).