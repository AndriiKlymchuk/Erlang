-module(cache_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Opts) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, Opts).

init(Opts) ->
	Server = 
        #{id      => cache_server,
          start   => {cache_server, start_link, [Opts]},
          restart => transient,
          modules => [gen_server]},
	{ok, {{one_for_one, 1, 5}, [Server]}}.
