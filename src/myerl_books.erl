-module(myerl_books).

-export([create/1, books/1, book/2, update/2, delete/2]).

create(Req) ->
    Book =
        jsx:decode(
            elli_request:body(Req)),
    Worker = poolboy:checkout(myerl_pool),
    ok =
        gen_server:call(Worker,
                        {query,
                         <<"insert into books(title, author) values(?, ?)">>,
                         [maps:get(<<"title">>, Book, <<"undefined">>),
                          maps:get(<<"author">>, Book, <<"undefined">>)],
                         no_filtermap_fun,
                         default_timeout}),
    poolboy:checkin(pool, Worker),
    {201, <<"Created">>}.

books(Req) ->
    Offset = elli_request:get_arg(<<"offset">>, Req, <<"0">>),
    Limit = elli_request:get_arg(<<"limit">>, Req, <<"100">>),
    Worker = poolboy:checkout(myerl_pool),
    {atomic, Result} =
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
    {200, [{<<"Content-Type">>, <<"application/json">>}], jsx:encode(Result)}.

book(BookId, _Req) ->
    Worker = poolboy:checkout(myerl_pool),
    {ok, ColumnNames, Rows} =
        gen_server:call(Worker,
                        {query,
                         <<"select id, title, author from books where id = ?">>,
                         [BookId],
                         no_filtermap_fun,
                         default_timeout}),
    poolboy:checkin(pool, Worker),
    case Rows of
        [Row] ->
            {200,
             [{<<"Content-Type">>, <<"application/json">>}],
             jsx:encode(to_map(ColumnNames, Row, #{}))};
        [] ->
            {404, <<"Not Found">>}
    end.

update(BookId, Req) ->
    Book =
        jsx:decode(
            elli_request:body(Req)),
    Worker = poolboy:checkout(myerl_pool),
    {atomic, AffectedRows} =
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
    case AffectedRows of
        1 ->
            {204, <<"No Content">>};
        _ ->
            {404, <<"Not Found">>}
    end.

delete(BookId, _Req) ->
    Worker = poolboy:checkout(myerl_pool),
    ok =
        gen_server:call(Worker,
                        {query,
                         <<"delete from books where id = ?">>,
                         [BookId],
                         no_filtermap_fun,
                         default_timeout}),
    poolboy:checkin(pool, Worker),
    {204, <<"No Content">>}.

to_map([ColumnName | ColumnNames], [Value | Values], Map) ->
    to_map(ColumnNames, Values, maps:put(ColumnName, Value, Map));
to_map([], [], Map) ->
    Map.
