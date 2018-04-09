-module(cache_server_handler).

-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([from_json/2]).

init(Req, State) ->
	{cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>, <<"OPTIONS">>], Req, State}.

content_types_accepted(Req, State) ->
	{[{<<"application/json">>, from_json}], Req, State}.

from_json(Req, State) ->
    {ok, JSON, _} = cowboy_req:read_body(Req),
    Ret =
        try jsone:decode(JSON) of
            Data ->
                handle(Data)
        catch
            error:badarg ->
                {error, <<"Malformed body">>}
        end,
    case Ret of
        {error, Error} ->
            Body = jsone:encode(#{<<"error">> => Error}),
            Req1 = cowboy_req:reply(400, #{}, Body, Req),
            {stop, Req1, State};
        Result ->
            Req1 = cowboy_req:set_resp_body(
                jsone:encode(#{<<"result">> => Result}),
                Req),
            {true, Req1, State}
    end.

%% Private
-spec handle(Data :: map()) -> {error, binary()} | binary().
handle(Data = #{<<"action">> := <<"insert">>,
                <<"key">>    := Key,
                <<"value">>  := Value}) ->
    Lifetime = maps:get(<<"lifetime">>, Data, infinity),
    if not is_number(Lifetime) andalso Lifetime =/= infinity ->
            {error, <<"Invalid lifetime">>};
        true ->
            cache_server:insert(Key, Value, Lifetime),
            <<"ok">>
    end;

handle(#{<<"action">> := <<"delete_all">>}) ->
    cache_server:delete_all(),
    <<"ok">>;

handle(#{<<"action">> := <<"lookup">>,
         <<"key">>    := Key}) ->
    case cache_server:lookup(Key) of
        {ok, Value} -> Value;
        not_found   -> null
    end;

handle(#{<<"action">> := <<"delete">>,
         <<"key">>    := Key}) ->
    cache_server:delete(Key),
    <<"ok">>;

handle(#{<<"action">>    := <<"lookup_by_date">>,
         <<"date_from">> := DateFrom,
         <<"date_to">>   := DateTo}) ->
    {ok, Pairs} = cache_server:lookup_by_date(DateFrom, DateTo),
    Pairs;

handle(_) ->
    {error, <<"Invalid request">>}.