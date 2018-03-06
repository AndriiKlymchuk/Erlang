-module(bs02).

-export([words/1]).

-spec(words(Bin :: binary()) ->
	[binary()]).
words(Bin) ->
	lists:reverse(words(Bin, [<<>>])).

-spec(words(Bin  :: binary(),
			Acc  :: list()) ->
	[binary()]).
words(<<" ", Rest/binary>>, Acc = [<<>>|_]) ->
	words(Rest, Acc);
words(<<" ", Rest/binary>>, Acc) ->
	words(Rest, [<<>>|Acc]);
words(<<Letter, Rest/binary>>, [Word|T]) ->
	words(Rest, [<<Word/binary, Letter>>|T]);
words(<<>>, [<<>>|T]) ->
	T;
words(<<>>, Acc) ->
	Acc.