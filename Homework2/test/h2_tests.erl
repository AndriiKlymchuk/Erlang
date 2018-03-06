-module(h2_tests).
-include_lib("eunit/include/eunit.hrl").

bs01_test_() ->
	[	
		?_assertEqual(<<"Some">>, bs01:first_word(<<"Some text">>)),
		?_assertEqual(<<"one">>, bs01:first_word(<<"one two three">>)),
		?_assertEqual(<<"a">>, bs01:first_word(<<"a b c">>)),
		?_assertEqual(<<"Word">>, bs01:first_word(<<"Word">>)),
		?_assertEqual(<<"Word">>, bs01:first_word(<<"  Word  ">>)),
		?_assertEqual(<<"a">>, bs01:first_word(<<"a">>)),
		?_assertEqual(<<>>, bs01:first_word(<<"   ">>)),
		?_assertEqual(<<>>, bs01:first_word(<<>>))
	].

bs02_test_() ->
	[	
		?_assertEqual([<<"Text">>, <<"with">>, <<"four">>, <<"words">>],
			bs02:words(<<"Text with four words">>)),
		?_assertEqual([<<"Two">>, <<"words">>], bs02:words(<<"Two words">>)),
		?_assertEqual([<<"Word">>], bs02:words(<<"Word">>)),
		?_assertEqual([<<"Word">>], bs02:words(<<"  Word  ">>)),
		?_assertEqual([<<"a">>, <<"b">>, <<"c">>], bs02:words(<<"a b c">>)),
		?_assertEqual([<<"a">>], bs02:words(<<"a">>)),
		?_assertEqual([], bs02:words(<<"   ">>)),
		?_assertEqual([], bs02:words(<<>>))
	].

bs03_test_() ->
	[	
		?_assertEqual([<<"Col1">>, <<"Col2">>, <<"Col3">>, <<"Col4">>, <<"Col5">>],
			bs03:split(<<"Col1-:-Col2-:-Col3-:-Col4-:-Col5">>, "-:-")),
		?_assertEqual([<<"Text">>, <<"with">>, <<"four">>, <<"words">>],
			bs03:split(<<"Text with four words">>, " ")),
		?_assertEqual([<<"Text with four words">>],
			bs03:split(<<"Text with four words">>, "")),
		?_assertEqual([<<"a">>, <<"b">>, <<"c">>], bs03:split(<<"a*b*c">>, "*")),
		?_assertEqual([<<"Word">>], bs03:split(<<"  Word  ">>, " ")),
		?_assertEqual([<<"-Word">>, <<"-">>], bs03:split(<<"---Word---">>, "--")),
		?_assertEqual([<<"Word">>], bs03:split(<<"Word">>, " ")),
		?_assertEqual([], bs03:split(<<>>, " "))
	].
