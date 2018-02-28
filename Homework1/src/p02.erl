-module(p02).

-export([but_last/1]).

%% Intentionally raise exception if list has less then two elements

-spec(but_last(L :: list()) ->
	list()).
but_last(L = [_, _ | []]) ->
	L;
but_last([_|T]) ->
	but_last(T).
	