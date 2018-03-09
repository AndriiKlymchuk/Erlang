-module(bs01).

-export([first_word/1]).

-spec(first_word(Bin :: binary()) ->
	binary()).
first_word(<<" ", Rest/binary>>) ->
	first_word(Rest);
first_word(Bin) ->
	case binary:match(Bin, <<" ">>) of
		{Len, _} ->
			<<FirstWord:Len/binary, _/binary>> = Bin,
			FirstWord;
		nomatch ->
			Bin
	end.