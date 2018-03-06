-module(bs01).

-export([first_word/1]).

-spec(first_word(Bin :: binary()) ->
	binary()).
first_word(<<" ", Rest/binary>>) ->
	first_word(Rest);
first_word(Bin) ->
	first_word(Bin, <<>>).

-spec(first_word(Bin :: binary(), Acc :: binary()) ->
	binary()).
first_word(<<" ", _/binary>>, Acc) ->
	Acc;
first_word(<<Letter, Rest/binary>>, Acc) ->
	first_word(Rest, <<Acc/binary, Letter>>);
first_word(<<>>, Acc) ->
	Acc.