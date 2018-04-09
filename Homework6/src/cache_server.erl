-module(cache_server).
-behaviour(gen_server).

%% API.
-export([start/1]).
-export([start_link/1]).
-export([stop/0]).
-export([insert/3]).
-export([lookup/1]).
-export([lookup_by_date/2]).
-export([delete/1]).
-export([delete_all/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {drop_interval=5 :: non_neg_integer()}).

-type opt() :: {drop_interval, non_neg_integer()}.

-include("cache.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

%% API.

-spec start(Opts :: [opt()]) -> {ok, pid()}.
start(Opts) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Opts, []).

-spec start_link(Opts :: [opt()]) -> {ok, pid()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

-spec stop() -> ok.
stop() ->
    gen_server:cast(?MODULE, stop).

-spec insert(Key      :: any(),
             Value    :: any(),
             Lifetime :: non_neg_integer() | infinity) -> ok.
insert(Key, Value, Lifetime) ->
    Now = current_time(),
    ExpiredAt = 
        if Lifetime == infinity ->
                infinity;
            true ->
                Now + Lifetime
        end,
    ets:insert(?TAB_NAME, {Key, Value, Now, ExpiredAt}),
    ok.

-spec lookup(Key :: any()) -> {ok, Value :: any()} | not_found. 
lookup(Key) ->
    Now = current_time(),
    case ets:lookup(?TAB_NAME, Key) of
        [{Key, Value, _, ExpiredAt}] when ExpiredAt > Now ->
           {ok, Value};
        _ ->
            not_found
    end.

-spec lookup_by_date(DateFrom :: calendar:datetime(),
                     DateTo   :: calendar:datetime()) ->
    {ok, Value :: [{any(), any()}]}. 
lookup_by_date(DateFrom, DateTo) ->
    DateFromSec = ?TO_SECS(DateFrom),
    DateToSec = ?TO_SECS(DateTo),
    Now = current_time(),
    Pairs = ets:select(?TAB_NAME, ets:fun2ms(
        fun({Key, Value, CreatedAt, ExpiredAt}) 
            when CreatedAt >= DateFromSec  andalso 
                 CreatedAt =< DateToSec    andalso 
                 ExpiredAt >  Now ->
            {Key, Value} 
        end)),
    {ok, Pairs}.

-spec delete(Key :: any()) -> ok. 
delete(Key) ->
    ets:delete(?TAB_NAME, Key),
    ok.

-spec delete_all() -> ok. 
delete_all() ->
    ets:delete_all_objects(?TAB_NAME),
    ok.

%% gen_server.

init(Opts) ->
    ets:new(?TAB_NAME, [named_table, public]),
    State = #state{drop_interval=DI} = prep_state(Opts),
    erlang:send_after(DI, self(), drop_obsolete),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(drop_obsolete, State) ->
    Now = current_time(),
    ets:select_delete(?TAB_NAME, ets:fun2ms(
      fun({_, _, CreatedAt, ExpiredAt})
        when ExpiredAt =< Now -> true end)),
    % update timer
    erlang:send_after(State#state.drop_interval, self(), drop_obsolete),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Private

-spec current_time() -> Seconds :: non_neg_integer().
current_time() ->
  ?TO_SECS(calendar:local_time()).

-spec prep_state(Opt :: [opt()]) -> #state{}.
prep_state(Opt) ->
    prep_state(Opt, #state{}).

-spec prep_state(Opt :: [opt()], #state{}) -> #state{}.
prep_state([{drop_interval, Val}|T], State) ->
    prep_state(T, State#state{drop_interval=Val * 1000});
prep_state([_|T], State) ->
    prep_state(T, State);
prep_state([], State) ->
    State.