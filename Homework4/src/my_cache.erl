-module(my_cache).

-export([
	create/0,
	insert/3,
	lookup/1,
	delete_obsolete/0
]).

-include_lib("stdlib/include/ms_transform.hrl").

-spec(create() -> ok | {error, alrady_created}).
create() ->
	try ets:new(?MODULE, [named_table]) of
		?MODULE -> ok
	catch
		error:badarg -> {error, already_created}
	end.

-spec(insert(Key 	  :: any(),
			 Value 	  :: any(),
			 LifeTime :: non_neg_integer()) -> ok).
insert(Key, Value, LifeTime) ->
	ExpirationTime = current_time() + LifeTime,
	try ets:insert(?MODULE, {Key, Value, ExpirationTime}) of
		true -> ok
	catch
		error:badarg -> {error, no_exists}
	end.

-spec(lookup(Key :: any()) ->
	{ok, Value :: any} |
	{error, undefined | no_exists}).
lookup(Key) ->
	Now = current_time(),
	try
		ets:select(?MODULE, ets:fun2ms(
			fun({RecKey, Value, ExpTime})
				when RecKey =:= Key, ExpTime > Now -> Value end))
	of
		[Value] -> {ok, Value};
		[] 		-> {error, undefined}
	catch
		error:badarg -> {error, no_exists}
	end.

-spec(delete_obsolete() -> ok | {error, not_exist}).
delete_obsolete() ->
	Now = current_time(),
	try
		ets:select_delete(?MODULE, ets:fun2ms(
			fun({_, _, ExpTime}) when ExpTime =< Now -> true end))
	of
		_ -> ok
	catch
		error:badarg -> {error, no_exists}
	end.

-spec(current_time() -> Seconds :: non_neg_integer()).
current_time() ->
	calendar:datetime_to_gregorian_seconds(calendar:local_time()).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-define(setup(F), {setup, fun setup/0, F}).

table_creation_test_() ->
	{"Cache table is created and next attempts of creation"
	 "lead to 'already_created' error",
	 ?setup(fun table_creation/1)}.

insertion_test_() ->
	{"Keys with values culd be inserted into table",
	 ?setup(fun table_insertion/1)}.

lookup_test_() ->
	{"Existing values could be retrieved",
	 ?setup(fun lookup_values/1)}.

lookup_old_test_() ->
	{"Old values ignored while lookup",
	 ?setup(fun lookup_old/1)}.

deelte_obsolete_test_() ->
	{"Obsolete pairs could be deleted",
	 ?setup(fun delete_old/1)}.

table_creation(_) ->
	TableInfo = ets:info(?MODULE),
	CreationRes1 = my_cache:create(),
	[?_assertNotEqual(undefined, TableInfo),
	 ?_assertEqual({error, already_created}, CreationRes1)].

table_insertion(_) ->
	InsertionRes0 = my_cache:insert(a, <<"a">>, 10),
	InsertionRes1 = my_cache:insert("b", 2, 100),
	TableContent = ets:match_object(?MODULE, {'_', '_', '_'}),
	[?_assertEqual(ok, InsertionRes0),
	 ?_assertEqual(ok, InsertionRes1),
	 ?_assertMatch([{a, <<"a">>, _}, {"b", 2, _}], TableContent)].

lookup_values(_) ->
	my_cache:insert(a, <<"a">>, 10),
	my_cache:insert("b", 2, 10),
	LookupRes0 = my_cache:lookup(a),
	LookupRes1 = my_cache:lookup("b"),
	[?_assertEqual({ok, <<"a">>}, LookupRes0),
	 ?_assertEqual({ok, 2}, LookupRes1)].

lookup_old(_) ->
	my_cache:insert(a, <<"a">>, 3),
	my_cache:insert("b", 2, 3),
	timer:sleep(4000),
	LookupRes0 = my_cache:lookup(a),
	LookupRes1 = my_cache:lookup("b"),
	TableContent = ets:match_object(?MODULE, {'_', '_', '_'}),
	[?_assertEqual({error, undefined}, LookupRes0),
	 ?_assertEqual({error, undefined}, LookupRes1),
	 ?_assertMatch([{a, <<"a">>, _}, {"b", 2, _}], TableContent)].

delete_old(_) ->
	my_cache:insert(a, <<"a">>, 3),
	my_cache:insert("b", 2, 3),
	my_cache:insert(5, {a}, 10),
	timer:sleep(4000),
	my_cache:delete_obsolete(),
	TableContent = ets:match_object(?MODULE, {'_', '_', '_'}),
	[?_assertMatch([{5, {a}, _}], TableContent)].

setup() ->
	my_cache:create().

-endif.