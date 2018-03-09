-module(bs02).

-export([words/1]).

-spec(words(Bin :: binary()) ->
	[binary()]).
words(Bin) ->
	lists:reverse(words(Bin, [])).

-spec(words(Bin  :: binary(),
			Acc  :: list()) ->
	[binary()]).
words(<<" ", Rest/binary>>, Acc) ->
	words(Rest, Acc);
words(Bin, Acc) ->
	case binary:match(Bin, <<" ">>) of
		{WordLen, _} ->
			<<Word:WordLen/binary, _:1/binary, Rest/binary>> = Bin,
			words(Rest, [Word|Acc]);
		nomatch ->
			if Bin == <<>> -> Acc; true -> [Bin|Acc] end
	end.
