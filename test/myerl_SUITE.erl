-module(myerl_SUITE).

-behaviour(ct_suite).

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([create/1, update/1, delete/1]).

-define(TITLE, <<"The Hobbit">>).
-define(AUTHOR, <<"J. R. R. Tolkien">>).

all() ->
    [create, update, delete].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(myerl),
    Config.

end_per_suite(Config) ->
    ok = application:stop(myerl),
    Config.

create(_Config) ->
    {ok, #{<<"id">> := BookId}} = myerl_books:create(?TITLE, ?AUTHOR),
    {ok, Book} = myerl_books:book(BookId),
    case Book of
        #{<<"id">> := BookId,
          <<"title">> := ?TITLE,
          <<"author">> := ?AUTHOR} ->
            ok;
        _ ->
            {failed,
             [{expected,
               #{<<"id">> => BookId,
                 <<"title">> => ?TITLE,
                 <<"author">> => ?AUTHOR}},
              {actual, Book}]}
    end.

update(_Config) ->
    {ok, #{<<"id">> := BookId}} = myerl_books:create(?TITLE, ?AUTHOR),
    ok = myerl_books:delete(BookId),
    no_row = myerl_books:book(BookId).

delete(_Config) ->
    NewTitle = <<"The Lord of the Rings">>,
    {ok, #{<<"id">> := BookId}} = myerl_books:create(?TITLE, ?AUTHOR),
    ok = myerl_books:update(BookId, NewTitle, ?AUTHOR),
    {ok, Book} = myerl_books:book(BookId),
    case Book of
        #{<<"id">> := BookId,
          <<"title">> := NewTitle,
          <<"author">> := ?AUTHOR} ->
            ok;
        _ ->
            {failed,
             [{expected,
               #{<<"id">> => BookId,
                 <<"title">> => NewTitle,
                 <<"author">> => ?AUTHOR}},
              {actual, Book}]}
    end.
