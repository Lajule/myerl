-module(myerl_router).

-behaviour(elli_handler).

%% api
-export([handle/2, handle_event/3]).

-define(CONTENT_TYPE, {<<"Content-Type">>, <<"application/json">>}).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.

%% ------------------------------------------------------------------
%% api
%% ------------------------------------------------------------------

handle(Req, _Args) ->
    handle(elli_request:method(Req), elli_request:path(Req), Req).

handle_event(_Event, _Data, _Args) ->
    ok.

%% ------------------------------------------------------------------
%% private api
%% ------------------------------------------------------------------

handle('GET', [<<"ping">>], _Req) ->
    {ok, [], <<"pong">>};
handle('GET', [<<"status">>], _Req) ->
    Infos = infos(),
    {ok, [?CONTENT_TYPE], jsx:encode(Infos)};
handle('POST', [<<"books">>], Req) ->
    Body = elli_request:body(Req),
    Book = jsx:decode(Body),
    Title = maps:get(<<"title">>, Book, <<"undefined">>),
    Author = maps:get(<<"author">>, Book, <<"undefined">>),
    Result = myerl_books:create(Title, Author),
    respond(Result);
handle('GET', [<<"books">>], Req) ->
    Offset = elli_request:get_arg(<<"offset">>, Req, <<"0">>),
    Limit = elli_request:get_arg(<<"limit">>, Req, <<"100">>),
    Result = myerl_books:books(Offset, Limit),
    respond(Result);
handle('GET', [<<"books">>, BookId], _Req) ->
    Result = myerl_books:book(BookId),
    respond(Result);
handle('PUT', [<<"books">>, BookId], Req) ->
    Body = elli_request:body(Req),
    Book = jsx:decode(Body),
    Title = maps:get(<<"title">>, Book, <<"undefined">>),
    Author = maps:get(<<"author">>, Book, <<"undefined">>),
    Result = myerl_books:update(BookId, Title, Author),
    respond(Result);
handle('DELETE', [<<"books">>, BookId], _Req) ->
    Result = myerl_books:delete(BookId),
    respond(Result);
handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.

infos() ->
    #{uptime => element(1, erlang:statistics(wall_clock)),
      memory => erlang:memory(),
      run_queue => erlang:statistics(run_queue),
      logical_processors => erlang:system_info(logical_processors),
      logical_processors_online => erlang:system_info(logical_processors_online),
      logical_processors_available => erlang:system_info(logical_processors_available),
      schedulers => erlang:system_info(schedulers),
      otp_release => erlang:system_info(otp_release),
      version => erlang:system_info(version),
      system_architecture => erlang:system_info(system_architecture),
      threads => erlang:system_info(threads),
      thread_pool_size => erlang:system_info(thread_pool_size),
      process_count => erlang:system_info(process_count)}.

respond(Result) ->
    case Result of
        ok ->
            {ok, [], <<"OK">>};
        {ok, Content} ->
            {ok, [?CONTENT_TYPE], jsx:encode(Content)};
        no_row ->
            {404, [], <<"Not Found">>}
    end.

%% ------------------------------------------------------------------
%% unit tests
%% ------------------------------------------------------------------

-ifdef(TEST).

response_test_() ->
    [?_assertEqual({ok, [], <<"OK">>}, respond(ok)),
     ?_assertEqual({ok, [?CONTENT_TYPE], jsx:encode(#{foo => <<"bar">>})},
                   respond({ok, #{foo => <<"bar">>}})),
     ?_assertEqual({404, [], <<"Not Found">>}, respond(no_row))].

-endif.
