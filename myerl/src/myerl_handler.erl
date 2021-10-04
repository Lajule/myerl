-module(myerl_handler).

-export([handle/2, handle_event/3]).

-behaviour(elli_handler).

handle(Req, _Args) ->
    handle(elli_request:method(Req), elli_request:path(Req), Req).

handle('GET', [], _Req) ->
    {ok, StatusCode, _RespHeaders, ClientRef} =
        hackney:request(get,
                        <<"http://ipinfo.io">>,
                        [{<<"Accept">>, <<"application/json">>}],
                        <<>>,
                        []),
    {ok, Body} = hackney:body(ClientRef),
    {StatusCode, [{<<"Content-Type">>, <<"application/json">>}], Body};
handle('POST', [<<"books">>], Req) ->
    myerl_books:create(Req);
handle('GET', [<<"books">>], Req) ->
    myerl_books:books(Req);
handle('GET', [<<"books">>, BookId], Req) ->
    myerl_books:book(BookId, Req);
handle('DELETE', [<<"books">>, BookId], Req) ->
    myerl_books:delete(BookId, Req);
handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.

handle_event(_Event, _Data, _Args) ->
    ok.
