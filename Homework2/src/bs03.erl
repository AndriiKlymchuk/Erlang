-module(bs03).

-export([split/2]).

-spec(split(Bin :: binary(), Del :: list() | binary()) ->
	[binary()]).
split(Bin, Del) when is_list(Del) ->
	split(Bin, list_to_binary(Del));
split(Bin, Del) ->
	lists:reverse(split(Bin, Del, size(Del), [<<>>])).

-spec(split(Bin  	:: binary(),
			Del  	:: binary(),
			DelSize :: non_neg_integer(),
			Acc  	:: list()) ->
	[binary()]).
split(Bin, <<>>, _, _) ->
	[Bin];
split(Bin, Del, DelSize, Acc = [Word|T]) ->
	case Bin of
		<<Del:DelSize/binary, Rest/binary>> when Word =:= <<>> ->
			split(Rest, Del, DelSize, Acc);
		<<Del:DelSize/binary, Rest/binary>> ->
			split(Rest, Del, DelSize, [<<>>|Acc]);
		<<Letter, Rest/binary>> ->
			split(Rest, Del, DelSize, [<<Word/binary, Letter>>|T]);
		<<>> when Word =:= <<>> ->
			T;
		<<>> ->
			Acc
	end.	