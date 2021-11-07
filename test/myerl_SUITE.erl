-module(myerl_SUITE).

-behaviour(ct_suite).

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([create/1]).

-define(TITLE, <<"The Hobbit">>).
-define(AUTHOR, <<"J. R. R. Tolkien">>).

all() ->
    [create].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(myerl),
    Config.

end_per_suite(Config) ->
    ok = application:stop(myerl),
    Config.

create(_Config) ->
    {ok, #{<<"id">> := Id}} = myerl_books:create(?TITLE, ?AUTHOR),
    {ok, Book} = myerl_books:book(Id),
    case Book of
        #{<<"id">> := Id,
          <<"title">> := ?TITLE,
          <<"author">> := ?AUTHOR} ->
            ok;
        _ ->
            {failed,
             [{expected,
               #{<<"id">> => Id,
                 <<"title">> => ?TITLE,
                 <<"author">> => ?AUTHOR}},
              {actual, Book}]}
    end.
