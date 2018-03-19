-module(h3_tests).
-include_lib("eunit/include/eunit.hrl").

json_parser_decode_as_map_test_() ->
	[
		?_assertEqual(#{}, json_parser:decode(<<"{}">>)),
		?_assertEqual(	
			#{
				<<"a">> => <<"string">>,
			  	<<"b">> => true,
				<<"c">> => false,
				<<"d">> => null,
				<<"e">> => [],
				<<"f">> => #{},
				<<"g">> => 234,
				<<"h">> => 56.50,
				<<"i">> => <<>>,
				<<"j">> => 0
			},
			json_parser:decode(
				<<"
				{
					\"a\":\"string\",
					\"b\":true,
					\"c\":false,
					\"d\":null,
					\"e\":[],
					\"f\":{},
					\"g\":234,
					\"h\":56.50,
					\"i\": \"\",
					\"j\": 0
				}
				">>
			)
		),
		?_assertEqual(	
			#{
				<<"abc">> => [
					<<"def">>, true, false, null, [], #{<<"a">> => true}, 0, 0.768
				],
				<<"-*-">> => #{
					<<"a">> => <<"b">>,
					<<"123">> => 123
				}
			},
			json_parser:decode(
				<<"
				{
					\"abc\" : [ \"def\", true, false, null, [], {\"a\":true}, 0, 0.768],
					\"-*-\" : {
						\"a\"   :    \"b\",
						\"123\" : 123
					}
				}
				">>
			)
		),
		?_assertEqual(	
			#{
				<<"a">> => #{<<"a">> => #{<<"a">> => #{<<"a">> => null}}},
				<<"b">> => [[[[0.56799]]]],
				<<"c">> => [#{<<"c">> => [#{<<"c">> => false}]}]
 			},
			json_parser:decode(
				<<"
				{
					\"a\": {\"a\": {\"a\": {\"a\": null}}},
					\"b\": [[[[0.56799]]]],
					\"c\": [{\"c\": [{\"c\": false}]}]
				}
				">>
			)
		),
		?_assertEqual({error, badarg}, json_parser:decode(<<"a">>)),
		?_assertEqual({error, badarg}, json_parser:decode(<<"{\"a\":}">>)),
		?_assertEqual({error, badarg}, json_parser:decode(<<"{\"a\":true,}">>)),
		?_assertEqual({error, badarg}, json_parser:decode(<<"{\"a\":true">>)),
		?_assertEqual({error, badarg}, json_parser:decode(<<"{\"a\":}">>)),
		?_assertEqual({error, badarg}, json_parser:decode(<<"{a:true}">>)),
		?_assertEqual({error, badarg}, json_parser:decode(<<"{\"a\"  \"b\"}">>))
	].

json_parser_decode_as_proplist_test_() ->
	[
		?_assertEqual([], json_parser:decode(<<"{}">>, [{format, proplist}])),
		?_assertEqual(	
			[
				{<<"a">>, <<"string">>},
			  	{<<"b">>, true},
				{<<"c">>, false},
				{<<"d">>, null},
				{<<"e">>, []},
				{<<"f">>, []},
				{<<"g">>, 234},
				{<<"h">>, 56.50},
				{<<"i">>, <<>>},
				{<<"j">>, 0}
			],
			json_parser:decode(
				<<"
				{
					\"a\":\"string\",
					\"b\":true,
					\"c\":false,
					\"d\":null,
					\"e\":[],
					\"f\":{},
					\"g\":234,
					\"h\":56.50,
					\"i\": \"\",
					\"j\": 0
				}
				">>
			,[{format, proplist}])
		),
		?_assertEqual(	
			[
				{<<"abc">>, [
					<<"def">>, true, false, null, [], [{<<"a">>, true}], 0, 0.768
				]},
				{<<"-*-">>, [
					{<<"a">>, <<"b">>},
					{<<"123">>, 123}
				]}
			],
			json_parser:decode(
				<<"
				{
					\"abc\" : [ \"def\", true, false, null, [], {\"a\":true}, 0, 0.768],
					\"-*-\" : {
						\"a\"   :    \"b\",
						\"123\" : 123
					}
				}
				">>
			,[{format, proplist}])
		),
		?_assertEqual(	
			[
				{<<"a">>, [{<<"a">>, [{<<"a">>, [{<<"a">>, null}]}]}]},
				{<<"b">>, [[[[0.56799]]]]},
				{<<"c">>, [[{<<"c">>, [[{<<"c">> , false}]]}]]}
 			],
			json_parser:decode(
				<<"
				{
					\"a\": {\"a\": {\"a\": {\"a\": null}}},
					\"b\": [[[[0.56799]]]],
					\"c\": [{\"c\": [{\"c\": false}]}]
				}
				">>
			,[{format, proplist}])
		),
		?_assertEqual({error, badarg},
			json_parser:decode(<<"a">>, [{format, proplist}])),
		?_assertEqual({error, badarg},
			json_parser:decode(<<"{\"a\":}">>, [{format, proplist}])),
		?_assertEqual({error, badarg},
			json_parser:decode(<<"{\"a\":true,}">>, [{format, proplist}])),
		?_assertEqual({error, badarg},
			json_parser:decode(<<"{\"a\":true">>, [{format, proplist}])),
		?_assertEqual({error, badarg},
			json_parser:decode(<<"{\"a\":}">>, [{format, proplist}])),
		?_assertEqual({error, badarg},
			json_parser:decode(<<"{a:true}">>, [{format, proplist}])),
		?_assertEqual({error, badarg},
			json_parser:decode(<<"{\"a\"  \"b\"}">>, [{format, proplist}]))
	]. 