-module(myerl_logger).

-export([handle/2, handle_event/3]).

-behaviour(elli_handler).

handle(Req, _Args) ->
    io:format("~p~n", [Req]),
    ignore.

handle_event(_Event, _Data, _Args) ->
    ok.
