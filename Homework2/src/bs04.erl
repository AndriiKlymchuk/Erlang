-module(bs04).

%% API
-export([decode_xml/1]).

-define(IS_WHITESPACE(Char),
	Char == $\s;
	Char == $\t;
	Char == $\n;
	Char == $\r
).
-define(IS_QUOTE(Char),
	Char == $";
	Char == $'
).

-type tag() :: {TagName :: binary(),
				Attrs 	:: list(),
				Content :: list()}.

%% ===================================================================
%% API functions
%% ===================================================================

-spec(decode_xml(Bin :: binary()) ->
	tag() | {error, badarg}).
decode_xml(Bin) ->
	try parse_tag(string:trim(Bin, leading, "\s\n\t\r")) of
		{DecodedXML, _} ->
			DecodedXML
	catch
		_:_ ->
			{error, badarg}
	end.
	
%% ===================================================================
%% Internal functions
%% ===================================================================

-spec(parse_tag(binary()) ->
	{tag(), Rest :: binary()}).
parse_tag(<<"<", Rest0/binary>>) ->
	[Tag0, Rest1] = binary:split(Rest0, <<">">>),
	Size = size(Tag0) - 1,
	case Tag0 of
		<<Tag1:Size/binary, "/">> ->	
			{TagName, Attrs} = parse_tag_inner(Tag1),
			{{TagName, Attrs, []}, Rest1};
		_ ->
			{TagName, Attrs} = parse_tag_inner(Tag0),
			{Content, Rest2} = parse_content(Rest1, TagName),
			{{TagName, Attrs, Content}, Rest2}
	end.

-spec(parse_tag_inner(Bin :: binary()) ->
	{TagName :: binary(), Attrs :: list()}).
parse_tag_inner(Bin) ->
	[TagName = <<_:1/binary, _/binary>>|Attrs0] =
		binary:split(Bin, [<<"\s">>, <<"\n">>, <<"\t">>, <<"\r">>]),
	Attrs1 = case Attrs0 of
				[] -> [];
				[Attrs] -> parse_attrs(Attrs)
			end,
	{TagName, Attrs1}.

-spec(parse_attrs(binary()) ->
	list()).
parse_attrs(<<Char, Rest/binary>>) when ?IS_WHITESPACE(Char) ->
	parse_attrs(Rest);
parse_attrs(<<>>) ->
	[];
parse_attrs(Attrs0) ->
	[Attr0, Rest0] = binary:split(Attrs0, <<"=">>),
	[Value, Rest1] = get_attr_value(Rest0),
	Attrs1 = parse_attrs(Rest1),
	[{string:trim(Attr0, trailing, "\s\n\t\r"), Value}|Attrs1].

-spec(get_attr_value(binary()) ->
	list()).
get_attr_value(<<Char, Rest/binary>>) when ?IS_WHITESPACE(Char) ->
	get_attr_value(Rest);
get_attr_value(<<Char, Rest0/binary>>) when ?IS_QUOTE(Char) ->
	binary:split(Rest0, <<Char>>).

-spec(parse_content(binary(), TagName :: binary()) ->
	{Content :: list(), Rest :: binary()}).
parse_content(<<"</", Rest0/binary>>, TagName) ->
	Size = size(TagName),
	<<TagName:Size/binary, Rest1/binary>> = Rest0,
	<<">", Rest2/binary>> = string:trim(Rest1, leading, "\s\n\t\r"),
	{[], Rest2};
parse_content(Tag = <<"<", _/binary>>, TagName) ->
	{Content1, Rest1} = parse_tag(Tag),
	{Content2, Rest2} = parse_content(Rest1, TagName),
	{[Content1|Content2], Rest2};
parse_content(Content, TagName) ->
	{Size, _} = binary:match(Content, <<"<">>),
	<<Content1:Size/binary, Rest1/binary>> = Content,
	{Content2, Rest2} = parse_content(Rest1, TagName),
	{[string:trim(Content1, both, "\s\n\t\r")|Content2], Rest2}.