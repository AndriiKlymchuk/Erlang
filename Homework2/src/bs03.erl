-module(bs03).

-export([split/2]).

-spec(split(Bin :: binary(), Del :: list() | binary()) ->
	[binary()]).
split(Bin, Del) when is_list(Del) ->
	split(Bin, list_to_binary(Del));
split(Bin, <<>>) ->
	[Bin];
split(Bin, Del) ->
	lists:reverse(split(Bin, Del, [])).

-spec(split(Bin  	:: binary(),
			Del  	:: binary(),
			Acc  	:: list()) ->
	[binary()]).
split(Bin, Del, Acc) ->
	case binary:match(Bin, Del) of
		{PLen, DLen} ->
			<<Part:PLen/binary, _:DLen/binary, Rest/binary>> = Bin,
			split(Rest, Del, [Part|Acc]);
		nomatch ->
			[Bin|Acc]
	end.	