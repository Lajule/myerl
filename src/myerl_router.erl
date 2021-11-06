-module(myerl_router).

-export([handle/2, handle_event/3]).

-behaviour(elli_handler).

-define(CONTENT_TYPE, {<<"Content-Type">>, <<"application/json">>}).

handle(Req, _Args) ->
    handle(elli_request:method(Req), elli_request:path(Req), Req).

handle('GET', [<<"ping">>], _Req) ->
    {ok, [], <<"pong">>};
handle('GET', [<<"status">>], _Req) ->
    {ok,
     [?CONTENT_TYPE],
     jsx:encode(
         myerl_status:infos())};
handle('POST', [<<"books">>], Req) ->
    Book =
        jsx:decode(
            elli_request:body(Req)),
    Title = maps:get(<<"title">>, Book, <<"undefined">>),
    Author = maps:get(<<"author">>, Book, <<"undefined">>),
    {ok,
     [?CONTENT_TYPE],
     jsx:encode(
         myerl_books:create(Title, Author))};
handle('GET', [<<"books">>], Req) ->
    Offset = elli_request:get_arg(<<"offset">>, Req, <<"0">>),
    Limit = elli_request:get_arg(<<"limit">>, Req, <<"100">>),
    {ok,
     [?CONTENT_TYPE],
     jsx:encode(
         myerl_books:create(Offset, Limit))};
handle('GET', [<<"books">>, BookId], _Req) ->
    Result = myerl_books:book(BookId),
    case Result of
        no_row ->
            {404, [], <<"Not Found">>};
        Map ->
            {ok, [?CONTENT_TYPE], jsx:encode(Map)}
    end;
handle('PUT', [<<"books">>, BookId], Req) ->
    Book =
        jsx:decode(
            elli_request:body(Req)),
    Title = maps:get(<<"title">>, Book, <<"undefined">>),
    Author = maps:get(<<"author">>, Book, <<"undefined">>),
    Result = myerl_books:update(BookId, Title, Author),
    case Result of
        ok ->
            {ok, [], <<"OK">>};
        no_row ->
            {404, [], <<"Not Found">>}
    end;
handle('DELETE', [<<"books">>, BookId], _Req) ->
    Result = myerl_books:delete(BookId),
    case Result of
        ok ->
            {ok, [], <<"OK">>};
        no_row ->
            {404, [], <<"Not Found">>}
    end;
handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.

handle_event(_Event, _Data, _Args) ->
    ok.
