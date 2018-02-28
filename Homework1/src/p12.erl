-module(p12).

-export([decode_modified/1]).

-spec(decode_modified(List :: list()) ->
	list()).
decode_modified([H|T]) ->
	p05:reverse(decode_modified(T, H, []));
decode_modified([]) ->
	[].

%% Infinite loop possible if number in tuple less than 1
%% Need to add guard when N > 1 to third clause

-spec(decode_modified(L   :: list(),
					  El  :: term(),
					  Acc :: list()) ->
	list()).

decode_modified([H|T], {1, Term}, Acc) ->
	decode_modified(T, H, [Term|Acc]);
decode_modified([], {1, Term}, Acc) ->
	[Term|Acc];
decode_modified(L, {N, Term}, Acc) ->
	decode_modified(L, {N - 1, Term}, [Term|Acc]);
decode_modified([H|T], Term, Acc) ->
	decode_modified(T, H, [Term|Acc]);
decode_modified([], Term, Acc) ->
	[Term|Acc].