-module(myerl_worker).

-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {pid}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
    {ok, Pid} = mysql:start_link(Args),
    {ok, #state{pid = Pid}}.

handle_call({query, Stmt, Params}, _From, #state{pid = Pid} = State) ->
    {reply, mysql:query(Pid, Stmt, Params), State};
handle_call({transaction, Fun}, _From, #state{pid = Pid} = State) ->
    {reply, mysql:transaction(Pid, Fun, [Pid], infinity), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{pid = Pid}) ->
    ok = mysql:stop(Pid),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
