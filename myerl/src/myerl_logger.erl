-module(myerl_logger).

-include_lib("kernel/include/logger.hrl").

-export([handle/2, handle_event/3]).

-behaviour(elli_handler).

handle(Req, Args) ->
    ?LOG_INFO(#{req => Req, args => Args}),
    ignore.

handle_event(_Event, _Data, _Args) ->
    ok.
