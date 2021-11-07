-module(myerl_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    PoolSpecs =
        poolboy:child_spec(myerl_pool,
                           application:get_all_env(poolboy)
                           ++ [{name, {local, myerl_pool}}, {worker_module, myerl_worker}],
                           application:get_all_env(mysql)),
    CallbackArgs =
        [{mods, [{elli_middleware_compress, []}, {myerl_logger, []}, {myerl_router, []}]}],
    ElliSpec =
        #{id => myerl_http,
          start =>
              {elli,
               start_link,
               [[{callback, elli_middleware}, {callback_args, CallbackArgs}]
                ++ application:get_all_env(elli)]},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [elli]},
    SupFlags =
        #{strategy => one_for_all,
          intensity => 0,
          period => 1},
    {ok, {SupFlags, [PoolSpecs, ElliSpec]}}.
