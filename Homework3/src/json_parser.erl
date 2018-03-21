-module(json_parser).

-export([decode/1
		,decode/2]).

-define(IS_DIGIT(C), C >= $0, C =< $9).

-type format() :: map | proplist.
-type opt()    :: {format, format()}.
-type key()    :: binary().
-type value()  :: binary() | list() | number() | object() | true | false | null.
-type object() :: #{key() => value()} | [{key(), value()}].

-record(opt, {format=map :: format()}).

-spec(decode(Bin :: binary()) ->
	object() | {error, badarg}).
decode(Bin) ->
	decode(Bin, []).

-spec(decode(Bin :: binary(), Opt :: list()) ->
	object() | {error, badarg}).
decode(Bin, Opt) ->
	try start_decode(Bin, Opt) of
		Obj -> Obj
	catch
		_:_ -> {error, badarg}
	end.

-spec(start_decode(Bin :: binary(), Opt :: list()) ->
	object()).
start_decode(Bin, Opt) ->
	<<"{", Rest0/binary>> = trim_left(Bin),
	{Obj, Rest1} = parse_object(trim_left(Rest0), prep_opt(Opt)),
	<<>> = trim_left(Rest1),
	Obj.

-spec(parse_object(binary(), #opt{}) ->
	{object(), Rest :: binary()}).
parse_object(<<"}", Rest/binary>>, Opt) ->
	{create_object([], Opt), Rest};
parse_object(Bin, Opt) ->
	parse_props(Bin, [], Opt).

-spec(parse_props(binary(), Acc :: list(), #opt{}) ->
	{object(), Rest :: binary()}).
parse_props(<<$", Rest0/binary>>, Acc, Opt) ->
	{Key = <<_:1/binary, _/binary>>, Rest1} = string(Rest0),
	<<":", Rest2/binary>> = trim_left(Rest1),
	{Value, Rest3} = parse_value(trim_left(Rest2), Opt),
	NewAcc = [{Key, Value}|Acc],
	case trim_left(Rest3) of
		<<",", Rest4/binary>> ->
			parse_props(trim_left(Rest4), NewAcc, Opt);
		<<"}", Rest4/binary>> ->
			{create_object(NewAcc, Opt), Rest4}
	end.

-spec(create_object(list(), #opt{}) -> object()).
create_object(L, #opt{format=map})      -> maps:from_list(L);
create_object(L, #opt{format=proplist}) -> lists:reverse(L).

-spec(parse_value(binary(), #opt{}) -> {value(), Rest :: binary()}).
parse_value(<<"false", Rest/binary>>, _) -> {false, Rest};
parse_value(<<"true", Rest/binary>>, _)  -> {true, Rest};
parse_value(<<"null", Rest/binary>>, _)  -> {null, Rest};
parse_value(<<$[, Rest/binary>>, Opt) -> parse_array(trim_left(Rest), Opt);
parse_value(<<${, Rest/binary>>, Opt) -> parse_object(trim_left(Rest), Opt);
parse_value(<<$", Rest/binary>>, _)   -> string(Rest);
parse_value(<<Bin/binary>>, _)        -> number(Bin).

-spec(parse_array(binary(), #opt{}) -> {list(), Rest :: binary()}).
parse_array(<<"]", Rest/binary>>, _) -> {[], Rest};
parse_array(<<Bin/binary>>, Opt)     -> parse_array(Bin, [], Opt).

-spec(parse_array(binary(), Acc :: list(), #opt{}) ->
	{list(), Rest :: binary()}).
parse_array(<<Bin/binary>>, Acc, Opt) ->
	{Value, Rest0} = parse_value(Bin, Opt),
	case trim_left(Rest0) of
		<<",", Rest1/binary>> ->
			parse_array(trim_left(Rest1), [Value|Acc], Opt);
		<<"]", Rest1/binary>> ->
			{lists:reverse([Value|Acc]), Rest1}
	end.

-spec(string(binary()) ->
	{String :: binary(), Rest :: binary()}).
string(Bin) ->
	{Len, _} = binary:match(Bin, <<"\"">>),
	<<String:Len/binary, _, Rest/binary>> = Bin,
	{String, Rest}.

-spec(number(binary()) -> {number(), Rest :: binary()}).
number(<<$-, Rest/binary>>) -> number_integer_part(Rest, -1);
number(Bin)                 -> number_integer_part(Bin, 1).

-spec(number_integer_part(binary(), Sign :: 1 | -1) ->
	{number(), Rest :: binary()}).
number_integer_part(<<$0, Rest/binary>>, Sign) ->
	number_fraction_part(Rest, 0, Sign);
number_integer_part(<<C, Rest/binary>>, Sign) when ?IS_DIGIT(C) ->
	number_integer_part_rest(Rest, C - $0, Sign).

-spec(number_integer_part_rest(binary(), Int :: integer(), Sign :: 1 | -1) ->
	{number(), Rest :: binary()}).
number_integer_part_rest(<<C, Rest/binary>>, Int, Sign) when ?IS_DIGIT(C) -> 
	number_integer_part_rest(Rest, Int * 10 + C - $0, Sign);
number_integer_part_rest(Bin, Int, Sign) ->
	number_fraction_part(Bin, Int, Sign).

-spec(number_fraction_part(binary(), Int :: integer(), Sign :: 1 | -1) ->
	{number(), Rest :: binary()}).
number_fraction_part(<<$., Rest/binary>>, Int, Sign) ->
	number_fraction_part_rest(Rest, Int, 0, Sign);
number_fraction_part(Bin, Int, Sign) ->
	{Int * Sign, Bin}.

-spec(number_fraction_part_rest(binary(), Num :: number(),
								DecimalsNum :: non_neg_integer(),
								Sign :: 1 | -1) ->
	{float(), Rest :: binary()}).
number_fraction_part_rest(<<C, Rest/binary>>,
						  Num, DecimalsNum, Sign) when ?IS_DIGIT(C) ->
	number_fraction_part_rest(Rest, Num * 10 + C - $0, DecimalsNum + 1, Sign);
number_fraction_part_rest(Bin, Num, DecimalsNum, Sign) when DecimalsNum > 0 ->
	{Num / math:pow(10, DecimalsNum) * Sign, Bin}. 

-spec(trim_left(binary()) -> binary()).
trim_left(<<"\s", Rest/binary>>) -> trim_left(Rest);
trim_left(<<"\t", Rest/binary>>) -> trim_left(Rest);
trim_left(<<"\n", Rest/binary>>) -> trim_left(Rest);
trim_left(<<"\r", Rest/binary>>) -> trim_left(Rest);
trim_left(Bin)                   -> Bin.

-spec(prep_opt([opt()]) -> #opt{}).
prep_opt(OptList) -> prep_opt(OptList, #opt{}).

-spec(prep_opt([opt()], #opt{}) -> #opt{}).
prep_opt([{format, F}|T], Opt) -> prep_opt(T, Opt#opt{format=F});
prep_opt([_|T], Opt)           -> prep_opt(T, Opt);
prep_opt([], Opt)              -> Opt.