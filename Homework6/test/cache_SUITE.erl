-module(cache_SUITE).
-include_lib("common_test/include/ct.hrl").

%% API
-export([all/0, init_per_testcase/2, end_per_testcase/2]).

%% TEST CASES
-export([lookup/1, lookup_obsolete/1, delete/1
        ,lookup_by_date/1, delete_all/1, drop_interval/1]).

-include("cache.hrl").

all() -> [
    lookup,
    lookup_obsolete,
    delete,
    lookup_by_date,
    delete_all,
    drop_interval
].

init_per_testcase(lookup, Config) ->
    ok = application:start(cache),
    cache_server:insert(key1, val1, 15),
    cache_server:insert(key2, val2, 20),
    cache_server:insert(key3, val3, 25),
    Config;
init_per_testcase(lookup_obsolete, Config) ->
    ok = application:start(cache),
    cache_server:insert(key1, val1, 1),
    cache_server:insert(key2, val2, 2),
    Config;
init_per_testcase(delete, Config) ->
    ok = application:start(cache),
    cache_server:insert(key1, val1, 10),
    cache_server:insert(key2, val2, 10),
    Config;
init_per_testcase(lookup_by_date, Config) ->
    ok = application:start(cache),
    DateFrom1 = calendar:local_time(),
    cache_server:insert(key1, val1, 30),
    cache_server:insert(key2, val2, 30),
    DateTo1 = calendar:local_time(),
    timer:sleep(3000),
    DateFrom2 = calendar:local_time(),
    cache_server:insert(key3, val3, 30),
    cache_server:insert(key4, val4, 30),
    DateTo2 = calendar:local_time(),
    [{dates, {{DateFrom1, DateTo1}, {DateFrom2, DateTo2}}} | Config];
init_per_testcase(delete_all, Config) ->
    ok = application:start(cache),
    DateFrom = calendar:local_time(),
    cache_server:insert(key1, val1, 30),
    cache_server:insert(key2, val2, 30),
    DateTo = calendar:local_time(),
    [{dates, {DateFrom, DateTo}} | Config];
init_per_testcase(drop_interval, Config) ->
    cache_server:start([{drop_interval, 10}]),
    cache_server:insert(key1, val1, 2),
    cache_server:insert(key2, val2, 15),
    Config.

end_per_testcase(drop_interval, _) ->
    cache_server:stop();
end_per_testcase(_, _) ->
    ok = application:stop(cache).

lookup(_) ->
    {ok, val1} = cache_server:lookup(key1),
    {ok, val2} = cache_server:lookup(key2),
    {ok, val3} = cache_server:lookup(key3),
    not_found  = cache_server:lookup(key4).

lookup_obsolete(_) ->
    timer:sleep(3000),
    not_found = cache_server:lookup(key1),
    not_found = cache_server:lookup(key2).

delete(_) ->
    {ok, val1} = cache_server:lookup(key1),
    ok = cache_server:delete(key1),
    not_found = cache_server:lookup(key1),
    {ok, val2} = cache_server:lookup(key2),
    ok = cache_server:delete(key2),
    not_found = cache_server:lookup(key2).

lookup_by_date(Config) ->
    {{DateFrom1, DateTo1}, {DateFrom2, DateTo2}} =
        ?config(dates, Config),
    {ok, [{key2, val2}, {key1, val1}]} =
        cache_server:lookup_by_date(DateFrom1, DateTo1),
    {ok, [{key4, val4}, {key3, val3}]} =
        cache_server:lookup_by_date(DateFrom2, DateTo2),
    {ok, [{key4, val4}, {key3, val3}, 
          {key2, val2}, {key1, val1}]} =
        cache_server:lookup_by_date(DateFrom1, DateTo2),
    DateFromSec = ?TO_SECS(calendar:local_time()) + 2,
    DateFrom3 = calendar:gregorian_seconds_to_datetime(DateFromSec),
    DateTo3 = calendar:gregorian_seconds_to_datetime(DateFromSec + 10),
    {ok, []} = cache_server:lookup_by_date(DateFrom3, DateTo3).

delete_all(Config) ->
    {DateFrom, DateTo} =
        ?config(dates, Config),
    {ok, [{key2, val2}, {key1, val1}]} =
        cache_server:lookup_by_date(DateFrom, DateTo),
    ok = cache_server:delete_all(),
    {ok, []} = cache_server:lookup_by_date(DateFrom, DateTo).

drop_interval(_) ->
    {ok, val1} = cache_server:lookup(key1),
    timer:sleep(2000),
    not_found = cache_server:lookup(key1),
    [{key1, val1, _, _}] = ets:lookup(?TAB_NAME, key1),
    timer:sleep(9000),
    [] = ets:lookup(?TAB_NAME, key1),
    {ok, val2} = cache_server:lookup(key2),
    timer:sleep(5000),
    not_found = cache_server:lookup(key2),
    [{key2, val2, _, _}] = ets:lookup(?TAB_NAME, key2),
    timer:sleep(8000),
    [] = ets:lookup(?TAB_NAME, key2).