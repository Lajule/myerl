-module(myerl_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    PoolSpecs =
        poolboy:child_spec(pool,
                           [{name, {local, pool}},
                            {worker_module, myerl_worker},
                            {size, 5},
                            {max_overflow, 10}],
                           [{host, "172.17.0.1"},
                            {user, "root"},
                            {password, "root"},
                            {database, "test"},
                            {port, 3306}]),
    ElliSpec =
        #{id => http,
          start => {elli, start_link, [[{callback, myerl_handler}, {port, 3000}]]},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [elli]},
    SupFlags =
        #{strategy => one_for_all,
          intensity => 0,
          period => 1},
    ChildSpecs = [PoolSpecs, ElliSpec],
    {ok, {SupFlags, ChildSpecs}}.
