-module(cache_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->  
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/cache_server", cache_server_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
        env => #{dispatch => Dispatch}
    }),
    {ok, Opts} = application:get_env(options),
	cache_sup:start_link(Opts).

stop(_State) ->
	ok.
