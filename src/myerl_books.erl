-module(myerl_books).

-export([create/1, books/1, book/2, update/2, delete/2]).

create(Req) ->
    Book =
        jsx:decode(
            elli_request:body(Req)),

    Worker = poolboy:checkout(myerl_pool),
    Result =
        gen_server:call(Worker,
                        {query,
                         <<"insert into books(title, author) values(?, ?)">>,
                         [maps:get(<<"title">>, Book, <<"undefined">>),
                          maps:get(<<"author">>, Book, <<"undefined">>)],
                         no_filtermap_fun,
                         default_timeout}),
    poolboy:checkin(pool, Worker),

    case Result of
        ok ->
            {201, <<"Created">>};
        _ ->
            {500, [], <<"Internal server error">>}
    end.

books(Req) ->
    Offset = elli_request:get_arg(<<"offset">>, Req, <<"0">>),
    Limit = elli_request:get_arg(<<"limit">>, Req, <<"100">>),

    Worker = poolboy:checkout(myerl_pool),
    Result =
        gen_server:call(Worker,
                        {transaction,
                         fun(Pid) ->
                            {ok, _, [[Total]]} =
                                mysql:query(Pid, <<"select count(*) as total from books">>),
                            {ok, ColumnNames, Rows} =
                                mysql:query(Pid,
                                            <<"select id, title, author from books limit ?, ?">>,
                                            [Offset, Limit]),
                            #{total => Total,
                              list => [to_map(ColumnNames, Row, #{}) || Row <- Rows]}
                         end,
                         [],
                         infinity}),
    poolboy:checkin(pool, Worker),

    case Result of
        {atomic, ResultOfFun} ->
            {200, [{<<"Content-Type">>, <<"application/json">>}], jsx:encode(ResultOfFun)};
        {aborted, _} ->
            {500, [], <<"Internal server error">>}
    end.

book(BookId, _Req) ->
    Worker = poolboy:checkout(myerl_pool),
    Result =
        gen_server:call(Worker,
                        {query,
                         <<"select id, title, author from books where id = ?">>,
                         [BookId],
                         no_filtermap_fun,
                         default_timeout}),
    poolboy:checkin(pool, Worker),

    case Result of
        {ok, ColumnNames, [Row]} ->
            {200,
             [{<<"Content-Type">>, <<"application/json">>}],
             jsx:encode(to_map(ColumnNames, Row, #{}))};
        [] ->
            {404, <<"Not Found">>};
        _ ->
            {500, [], <<"Internal server error">>}
    end.

update(BookId, Req) ->
    Book =
        jsx:decode(
            elli_request:body(Req)),

    Worker = poolboy:checkout(myerl_pool),
    Result =
        gen_server:call(Worker,
                        {transaction,
                         fun(Pid) ->
                            ok =
                                mysql:query(Pid,
                                            "update books set title = ?, author = ? where id = ?",
                                            [maps:get(<<"title">>, Book, <<"undefined">>),
                                             maps:get(<<"author">>, Book, <<"undefined">>),
                                             BookId]),
                            mysql:affected_rows(Pid)
                         end,
                         [],
                         infinity}),
    poolboy:checkin(pool, Worker),

    case Result of
        {atomic, 1} ->
            {204, <<"No Content">>};
        {atomic, 0} ->
            {404, <<"Not Found">>};
        {aborted, _} ->
            {500, [], <<"Internal server error">>}
    end.

delete(BookId, _Req) ->
    Worker = poolboy:checkout(myerl_pool),
    Result =
        gen_server:call(Worker,
                        {query,
                         <<"delete from books where id = ?">>,
                         [BookId],
                         no_filtermap_fun,
                         default_timeout}),
    poolboy:checkin(pool, Worker),

    case Result of
        ok ->
            {204, <<"No Content">>};
        _ ->
            {500, [], <<"Internal server error">>}
    end.

to_map([ColumnName | ColumnNames], [Value | Values], Map) ->
    to_map(ColumnNames, Values, maps:put(ColumnName, Value, Map));
to_map([], [], Map) ->
    Map.
