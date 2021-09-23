-module(myerl_handler).

-export([handle/2, handle_event/3]).

-behaviour(elli_handler).

handle(Req, _Args) ->
    handle(elli_request:method(Req), elli_request:path(Req), Req).

handle('POST', [<<"books">>], Req) ->
    Title = elli_request:post_arg_decoded(<<"title">>, Req, <<"undefined">>),
    Author = elli_request:post_arg_decoded(<<"author">>, Req, <<"undefined">>),
    Worker = poolboy:checkout(myerl_pool),
    ok =
        gen_server:call(Worker,
                        {query,
                         <<"insert into books(title, author) values(?, ?)">>,
                         [Title, Author]}),
    poolboy:checkin(pool, Worker),
    {201, [], <<"Created">>};
handle('GET', [<<"books">>], Req) ->
    Offset = elli_request:get_arg(<<"offset">>, Req, <<"0">>),
    Limit = elli_request:get_arg(<<"limit">>, Req, <<"100">>),
    Worker = poolboy:checkout(myerl_pool),
    {ok, _, Rows} =
        gen_server:call(Worker,
                        {query,
                         <<"select id, title, author from books limit ?, ?">>,
                         [Offset, Limit]}),
    poolboy:checkin(pool, Worker),
    {200, [{<<"Content-Type">>, <<"application/json">>}], jsx:encode(Rows)};
handle('GET', [<<"books">>, BookId], _Req) ->
    Worker = poolboy:checkout(myerl_pool),
    {ok, _, Rows} =
        gen_server:call(Worker,
                        {query, <<"select id, title, author from books where id = ?">>, [BookId]}),
    poolboy:checkin(pool, Worker),
    case Rows of
	[Row] -> {200, [{<<"Content-Type">>, <<"application/json">>}], jsx:encode(Row)};
        [] -> {204, <<"No Content">>}
    end;
handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.

handle_event(_Event, _Data, _Args) ->
    ok.
