-module(p01).

-export([last/1]).

%% Intentionally raise exception if list is empty

-spec(last(L :: list()) ->
	term()).
last([H]) ->
	H;
last([_|T]) ->
	last(T).