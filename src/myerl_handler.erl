-module(myerl_handler).

-export([handle/2, handle_event/3]).

-behaviour(elli_handler).

handle(Req, _Args) ->
    handle(elli_request:method(Req), elli_request:path(Req), Req).

handle('GET', [<<"users">>], _Req) ->
    Worker = poolboy:checkout(myerl_pool),
    {ok, _, Rows} = gen_server:call(Worker, {query, <<"select * from users">>, []}),
    poolboy:checkin(pool, Worker),
    {200, [{<<"Content-Type">>, <<"application/json">>}], jsx:encode(Rows)};
handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.

handle_event(_Event, _Data, _Args) ->
    ok.
