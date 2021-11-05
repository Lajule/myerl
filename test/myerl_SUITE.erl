-module(myerl_SUITE).

-behaviour(ct_suite).

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([create/1]).

all() ->
    [create].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(myerl),
    Config.

end_per_suite(Config) ->
    ok = application:stop(myerl),
    Config.

create(_Config) ->
    myerl_books:create(<<"undefined">>, <<"undefined">>).
