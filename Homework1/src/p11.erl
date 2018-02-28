-module(p11).

-export([encode_modified/1]).

-spec(encode_modified(L :: list()) ->
	list()).
encode_modified([H|T]) ->
	p05:reverse(encode_modified(T, H, []));
encode_modified([]) ->
	[].

-spec(encode_modified(L   :: list(),
					  El  :: term(),
					  Acc :: list()) ->
	[{pos_integer(), term()} | term()]).
encode_modified([H|T], {N, H}, Acc) ->
	encode_modified(T, {N + 1, H}, Acc);
encode_modified([H|T], H, Acc) ->
	encode_modified(T, {2, H}, Acc);
encode_modified([H|T], El, Acc) ->
	encode_modified(T, H, [El|Acc]);
encode_modified([], El, Acc) ->
	[El|Acc].