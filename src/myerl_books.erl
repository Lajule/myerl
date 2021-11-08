-module(myerl_books).

%% api
-export([create/2, books/3, book/1, update/3, delete/1]).

-include_lib("kernel/include/logger.hrl").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.

%% ------------------------------------------------------------------
%% api
%% ------------------------------------------------------------------

create(Title, Author) ->
    Worker = poolboy:checkout(myerl_pool),
    Result =
        gen_server:call(Worker,
                        {transaction,
                         fun(Pid) ->
                            ok =
                                mysql:query(Pid,
                                            <<"INSERT INTO books(title, author) VALUES(?, ?)">>,
                                            [Title, Author]),
                            mysql:insert_id(Pid)
                         end,
                         [],
                         infinity}),
    poolboy:checkin(pool, Worker),
    case Result of
        {atomic, Id} ->
            NewBook = #{<<"id">> => Id},
            {ok, NewBook};
        {aborted, Reason} ->
            ?LOG_ERROR("error happened because: ~p", [Reason]),
            throw(db_error)
    end.

books(undefined, Offset, Limit) ->
    books_query({<<"SELECT COUNT(*) AS total FROM books">>, []},
                {<<"SELECT id, title, author FROM books LIMIT ?, ?">>, [Offset, Limit]});
books(Search, Offset, Limit) ->
    books_query({<<"SELECT COUNT(*) AS total FROM books "
                   "WHERE MATCH(title, author) AGAINST(? IN BOOLEAN MODE)">>,
                 [Search]},
                {<<"SELECT id, title, author FROM books "
                   "WHERE MATCH(title, author) AGAINST(? IN BOOLEAN MODE) "
                   "LIMIT ?, ?">>,
                 [Search, Offset, Limit]}).

book(BookId) ->
    Worker = poolboy:checkout(myerl_pool),
    Result =
        gen_server:call(Worker,
                        {query,
                         <<"SELECT id, title, author FROM books WHERE id = ?">>,
                         [BookId],
                         no_filtermap_fun,
                         default_timeout}),
    poolboy:checkin(pool, Worker),
    case Result of
        {ok, ColumnNames, [Row]} ->
            Book = row_to_map(ColumnNames, Row, #{}),
            {ok, Book};
        {ok, _, []} ->
            no_row;
        {error, Reason} ->
            ?LOG_ERROR("error happened because: ~p", [Reason]),
            throw(db_error)
    end.

update(BookId, Title, Author) ->
    Worker = poolboy:checkout(myerl_pool),
    Result =
        gen_server:call(Worker,
                        {transaction,
                         fun(Pid) ->
                            ok =
                                mysql:query(Pid,
                                            "UPDATE books SET title = ?, author = ? WHERE id = ?",
                                            [Title, Author, BookId]),
                            mysql:affected_rows(Pid)
                         end,
                         [],
                         infinity}),
    poolboy:checkin(pool, Worker),
    case Result of
        {atomic, 1} ->
            ok;
        {atomic, 0} ->
            no_row;
        {aborted, Reason} ->
            ?LOG_ERROR("error happened because: ~p", [Reason]),
            throw(db_error)
    end.

delete(BookId) ->
    Worker = poolboy:checkout(myerl_pool),
    Result =
        gen_server:call(Worker,
                        {transaction,
                         fun(Pid) ->
                            ok = mysql:query(Pid, "DELETE FROM books WHERE id = ?", [BookId]),
                            mysql:affected_rows(Pid)
                         end,
                         [],
                         infinity}),
    poolboy:checkin(pool, Worker),
    case Result of
        {atomic, 1} ->
            ok;
        {atomic, 0} ->
            no_row;
        {aborted, Reason} ->
            ?LOG_ERROR("error happened because: ~p", [Reason]),
            throw(db_error)
    end.

%% ------------------------------------------------------------------
%% private api
%% ------------------------------------------------------------------

books_query({CountQuery, CountArgs}, {SelectQuery, SelectArgs}) ->
    Worker = poolboy:checkout(myerl_pool),
    Result =
        gen_server:call(Worker,
                        {transaction,
                         fun(Pid) ->
                            {ok, _, [[Total]]} = mysql:query(Pid, CountQuery, CountArgs),
                            {ok, ColumnNames, Rows} = mysql:query(Pid, SelectQuery, SelectArgs),
                            BookList = [row_to_map(ColumnNames, Row, #{}) || Row <- Rows],
                            #{total => Total, list => BookList}
                         end,
                         [],
                         infinity}),
    poolboy:checkin(pool, Worker),
    case Result of
        {atomic, BookList} ->
            {ok, BookList};
        {aborted, Reason} ->
            ?LOG_ERROR("error happened because: ~p", [Reason]),
            throw(db_error)
    end.

row_to_map([ColumnName | ColumnNames], [Value | Values], Map) ->
    row_to_map(ColumnNames, Values, maps:put(ColumnName, Value, Map));
row_to_map([], [], Map) ->
    Map.

%% ------------------------------------------------------------------
%% unit tests
%% ------------------------------------------------------------------

-ifdef(TEST).

row_to_map_test_() ->
    ?_assertEqual(#{foo => <<"bar">>}, row_to_map([foo], [<<"bar">>], #{})).

-endif.
