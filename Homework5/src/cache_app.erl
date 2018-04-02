-module(cache_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    {ok, Opts} = application:get_env(options),
	cache_sup:start_link(Opts).

stop(_State) ->
	ok.
