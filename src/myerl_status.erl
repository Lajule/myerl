-module(myerl_status).

-export([infos/1, ping/1]).

infos(_Req) ->
    Infos =
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
          process_count => erlang:system_info(process_count)},
    {200, [{<<"Content-Type">>, <<"application/json">>}], jsx:encode(Infos)}.

ping(_Req) ->
    {200, [], <<"pong">>}.
