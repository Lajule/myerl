-module(myerl_books).

%% api
-export([create/2, books/2, book/1, update/3, delete/1]).

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
                                            <<"insert into books(title, author) values(?, ?)">>,
                                            [Title, Author]),
                            mysql:insert_id(Pid)
                         end,
                         [],
                         infinity}),
    poolboy:checkin(pool, Worker),
    case Result of
        {atomic, Id} ->
            NewBook = #{id => Id},
            {ok, NewBook};
        {aborted, Reason} ->
            ?LOG_ERROR("error happened because: ~p", [Reason]),
            throw(db_error)
    end.

books(Offset, Limit) ->
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
                              list => [row_to_map(ColumnNames, Row, #{}) || Row <- Rows]}
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

book(BookId) ->
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
                                            "update books set title = ?, author = ? where id = ?",
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
                            ok = mysql:query(Pid, "delete from books where id = ?", [BookId]),
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
