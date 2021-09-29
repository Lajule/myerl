-module(myerl_books).

-include_lib("kernel/include/logger.hrl").

-export([create/1, books/1, book/2, delete/2]).

create(Req) ->
    Title = elli_request:post_arg_decoded(<<"title">>, Req, <<"undefined">>),
    Author = elli_request:post_arg_decoded(<<"author">>, Req, <<"undefined">>),
    Worker = poolboy:checkout(myerl_pool),
    ok =
        gen_server:call(Worker,
                        {query,
                         <<"insert into books(title, author) values(?, ?)">>,
                         [Title, Author]}),
    poolboy:checkin(pool, Worker),
    {201, [], <<"Created">>}.

books(Req) ->
    Offset = elli_request:get_arg(<<"offset">>, Req, <<"0">>),
    Limit = elli_request:get_arg(<<"limit">>, Req, <<"100">>),
    Worker = poolboy:checkout(myerl_pool),
    {ok, ColumnNames, Rows} =
        gen_server:call(Worker,
                        {query,
                         <<"select id, title, author from books limit ?, ?">>,
                         [Offset, Limit]}),
    poolboy:checkin(pool, Worker),
    case Rows of
        [Row] ->
	    List = [to_map(ColumnNames, Row, #{}) || Row <- Rows],
	    {200, [{<<"Content-Type">>, <<"application/json">>}], jsx:encode(List)};
        [] ->
            {204, [], <<"No Content">>}
    end.

book(BookId, _Req) ->
    Worker = poolboy:checkout(myerl_pool),
    {ok, ColumnNames, Rows} =
        gen_server:call(Worker,
                        {query, <<"select id, title, author from books where id = ?">>, [BookId]}),
    poolboy:checkin(pool, Worker),
    case Rows of
        [Row] ->
            {200,
             [{<<"Content-Type">>, <<"application/json">>}],
             jsx:encode(to_map(ColumnNames, Row, #{}))};
        [] ->
            {404, [], <<"Not Found">>}
    end.

delete(BookId, _Req) ->
    Worker = poolboy:checkout(myerl_pool),
    ok = gen_server:call(Worker, {query, <<"delete from books where id = ?">>, [BookId]}),
    poolboy:checkin(pool, Worker),
    {204, <<"No Content">>}.

to_map([ColumnName | ColumnNames], [Value | Values], Map) ->
    to_map(ColumnNames, Values, maps:put(ColumnName, Value, Map));
to_map([], [], Map) ->
    Map.
