-module(key_value_bench).

-export([all_tests/1,
		insertions_tests/1,
		lookup_tests/1,
		update_tests/1]).

-define(VALUE, <<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15>>).
-define(NEW_VALUE, <<1,1,1,1,1,1,1,1,1,1,1,1,1,1,1>>).
-define(REPEAT, 1000).


all_tests(Size) ->
	insertions_tests(Size),
	io:format("~n------------------~n~n"), 
	lookup_tests(Size),
	io:format("~n------------------~n~n"),
	update_tests(Size).

insertions_tests(Size) ->
	Keys = shuffle_list(lists:seq(1, Size)),
	io:format("Insertion tests:~n"),
	ets_set_insertions(Keys),
	ets_ordered_set_insertions(Keys),
	ets_bag_insertions(Keys),
	ets_duplicate_bag_insertions(Keys),
	maps_put_insertions(Keys),
	maps_insertions(Keys),
	process_dict_insertions(Keys),
	ok.

lookup_tests(Size) -> 
	Keys = shuffle_list(lists:seq(1, Size)),
	io:format("Lookup tests:~n"),
	ets_set_lookup(Keys),
	ets_ordered_set_lookup(Keys),
	ets_bag_lookup(Keys),
	ets_duplicate_bag_lookup(Keys),
	maps_get_lookup(Keys),
	maps_pm_lookup(Keys),
	process_dict_lookup(Keys),
	ok.

update_tests(Size) -> 
	Keys = shuffle_list(lists:seq(1, Size)),
	io:format("Update tests:~n"),
	ets_set_update(Keys),
	ets_ordered_set_update(Keys),
	maps_put_update(Keys),
	maps_update(Keys),
	process_dict_update(Keys),
	ok.

%% INSERT

ets_set_insertions(Keys) ->
	ets:new(test, [named_table, set]),
	bench("ets set",
		fun() -> insert_into_ets(Keys) end,
		?REPEAT),
	ets:delete(test).

ets_ordered_set_insertions(Keys) ->
	ets:new(test, [named_table, ordered_set]),
	bench("ets ordered_set",
		fun() -> insert_into_ets(Keys) end,
		?REPEAT),
	ets:delete(test).

ets_duplicate_bag_insertions(Keys) ->
	ets:new(test, [named_table, duplicate_bag]),
	bench("ets duplicate_bag",
		fun() -> insert_into_ets(Keys) end,
		?REPEAT),
	ets:delete(test).

ets_bag_insertions(Keys) ->
	ets:new(test, [named_table, bag]),
	bench("ets bag",
		fun() -> insert_into_ets(Keys) end,
		?REPEAT),
	ets:delete(test).

maps_put_insertions(Keys) ->
	bench("maps put",
		fun() -> insert_with_put_into_map(#{}, Keys) end,
		?REPEAT).

maps_insertions(Keys) ->
	bench("maps",
		fun() -> insert_into_map(#{}, Keys) end,
		?REPEAT).

process_dict_insertions(Keys) ->
	bench("process dict",
		fun() -> insert_into_process_dict(Keys) end,
		?REPEAT),
	erase().

insert_into_ets([Key|Rest]) ->
	ets:insert(test, {Key, ?VALUE}),
	insert_into_ets(Rest);
insert_into_ets([]) -> ok.

insert_with_put_into_map(Map, [Key|Rest]) ->
	insert_with_put_into_map(maps:put(Key, ?VALUE, Map), Rest);
insert_with_put_into_map(Map, []) -> Map.

insert_into_map(Map, [Key|Rest]) ->
	insert_into_map(Map#{Key => ?VALUE}, Rest);
insert_into_map(Map, []) -> Map.

insert_into_process_dict([Key|Rest]) ->
	put(Key, ?VALUE),
	insert_into_process_dict(Rest);
insert_into_process_dict([]) -> ok.


%% LOOKUP

ets_set_lookup(Keys0) ->
	ets:new(test, [named_table, set]),
	insert_into_ets(Keys0),
	Keys1 = shuffle_list(Keys0),
	bench("ets set",
		fun() -> lookup_in_ets(Keys1) end,
		?REPEAT),
	ets:delete(test).

ets_ordered_set_lookup(Keys0) ->
	ets:new(test, [named_table, ordered_set]),
	insert_into_ets(Keys0),
	Keys1 = shuffle_list(Keys0),
	bench("ets ordered_set",
		fun() -> lookup_in_ets(Keys1) end,
		?REPEAT),
	ets:delete(test).

ets_duplicate_bag_lookup(Keys0) ->
	ets:new(test, [named_table, duplicate_bag]),
	insert_into_ets(Keys0),
	Keys1 = shuffle_list(Keys0),
	bench("ets duplicate_bag",
		fun() -> lookup_in_ets(Keys1) end,
		?REPEAT),
	ets:delete(test).

ets_bag_lookup(Keys0) ->
	ets:new(test, [named_table, bag]),
	insert_into_ets(Keys0),
	Keys1 = shuffle_list(Keys0),
	bench("ets bag",
		fun() -> lookup_in_ets(Keys1) end,
		?REPEAT),
	ets:delete(test).

maps_get_lookup(Keys0) ->
	Map = insert_with_put_into_map(#{}, Keys0),
	Keys1 = shuffle_list(Keys0),
	bench("maps get",
		fun() -> lookup_with_get_in_map(Map, Keys1) end,
		?REPEAT).

maps_pm_lookup(Keys0) ->
	Map = insert_with_put_into_map(#{}, Keys0),
	Keys1 = shuffle_list(Keys0),
	bench("maps",
		fun() -> lookup_in_map(Map, Keys1) end,
		?REPEAT).

process_dict_lookup(Keys0) ->
	insert_into_process_dict(Keys0),
	Keys1 = shuffle_list(Keys0),
	bench("process dict",
		fun() -> lookup_in_process_dict(Keys1) end,
		?REPEAT),
	erase().

lookup_in_ets([Key|Rest]) ->
	ets:lookup(test, Key),
	lookup_in_ets(Rest);
lookup_in_ets([]) -> ok.

lookup_with_get_in_map(Map, [Key|Rest]) ->
	maps:get(Key, Map),
	lookup_with_get_in_map(Map, Rest);
lookup_with_get_in_map(_, []) -> ok.

lookup_in_map(Map, [Key|Rest]) ->
	#{Key := _} = Map,
	lookup_in_map(Map, Rest);
lookup_in_map(_, []) -> ok.

lookup_in_process_dict([Key|Rest]) ->
	get(Key),
	lookup_in_process_dict(Rest);
lookup_in_process_dict([]) -> ok.

%% UPDATE

ets_set_update(Keys0) ->
	ets:new(test, [named_table, set]),
	insert_into_ets(Keys0),
	Keys1 = shuffle_list(Keys0),
	bench("ets set",
		fun() -> insert_into_ets(Keys1) end,
		?REPEAT),
	ets:delete(test).

ets_ordered_set_update(Keys0) ->
	ets:new(test, [named_table, ordered_set]),
	insert_into_ets(Keys0),
	Keys1 = shuffle_list(Keys0),
	bench("ets ordered_set",
		fun() -> insert_into_ets(Keys1) end,
		?REPEAT),
	ets:delete(test).

maps_put_update(Keys0) ->
	Map = insert_with_put_into_map(#{}, Keys0),
	Keys1 = shuffle_list(Keys0),
	bench("maps put",
		fun() -> update_with_put_map(Map, Keys1) end,
		?REPEAT).

maps_update(Keys0) ->
	Map = insert_with_put_into_map(#{}, Keys0),
	Keys1 = shuffle_list(Keys0),
	bench("maps",
		fun() -> update_map(Map, Keys1) end,
		?REPEAT).

process_dict_update(Keys0) ->
	insert_into_process_dict(Keys0),
	Keys1 = shuffle_list(Keys0),
	bench("process dict",
		fun() -> update_process_dict(Keys1) end,
		?REPEAT),
	erase().


update_with_put_map(Map, [Key|Rest]) ->
	update_with_put_map(maps:put(Key, ?NEW_VALUE, Map), Rest);
update_with_put_map(_, []) -> ok.

update_map(Map, [Key|Rest]) ->
	update_map(maps:update(Key, ?NEW_VALUE, Map), Rest);
update_map(_, []) -> ok.

update_process_dict([Key|Rest]) ->
	put(Key, ?NEW_VALUE),
	update_process_dict(Rest);
update_process_dict([]) -> ok.

%% HELPERS

shuffle_list(L) ->
	[X||{_,X} <- lists:sort([ {rand:uniform(), N} || N <- L])].

bench(Name, Fun, Trials) ->
    print_result(Name, repeat_tc(Fun, Trials)).

repeat_tc(Fun, Trials) ->
    {Time, _} = timer:tc(fun() -> repeat(Trials, Fun) end),
    {Time, Trials}.

repeat(0, _Fun) -> ok;
repeat(N, Fun) -> Fun(), repeat(N - 1, Fun).

print_result(Name, {Time, Trials}) ->
    io:format("~s: ~.3f us (~.2f per second)~n",
              [Name, Time / Trials, Trials / (Time / 1000000)]).