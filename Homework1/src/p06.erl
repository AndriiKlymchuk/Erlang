-module(p06).

-export([is_palindrome/1]).

-spec(is_palindrome(L :: list()) ->
	boolean()).
is_palindrome(L) ->
	L =:= p05:reverse(L).

%% Longer solution without p05:reverse/1
%%
%% -spec(is_palindrome(L :: list()) ->
%%	boolean()).
%% is_palindrome(L) ->
%%	is_palindrome(L, L, []).
%%
%% -spec(is_palindrome(InitL   :: list(),
%%					   ReduceL :: list(),
%%					   RevL	   :: list()) ->
%%	boolean()).
%% is_palindrome(InitL, [H|T], RevL) ->
%% 	is_palindrome(InitL, T, [H|RevL]);
%% is_palindrome(InitL, [], RevL) ->
%%	InitL =:= RevL.