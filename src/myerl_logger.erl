-module(myerl_logger).

-compile([{parse_transform, lager_transform}]).

-export([handle/2, handle_event/3]).

-behaviour(elli_handler).

handle(_Req, _Args) ->
    ignore.

handle_event(request_complete,
             [Req, ResponseCode, _ResponseHeaders, ResponseBody, {Timings, _Sizes}],
             _Args) ->
    RequestStart = proplists:get_value(request_start, Timings),
    RequestEnd = proplists:get_value(request_end, Timings),
    lager:info("Request complete ~p",
               [#{req => elli_request:peer(Req),
                  time => erlang:convert_time_unit(RequestEnd - RequestStart, native, millisecond),
                  response_code => ResponseCode,
                  response_body => erlang:iolist_size(ResponseBody),
                  method => elli_request:method(Req),
                  path => elli_request:raw_path(Req)}]),
    ok;
handle_event(_Event, _Data, _Args) ->
    ok.
