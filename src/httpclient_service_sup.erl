-module(httpclient_service_sup).

-behaviour(supervisor).

%% User functions
-export([start_link/1]).

%% Behavior callbacks
-export([init/1]).

%% ============================================================================
%% User functions
%% ============================================================================

start_link(PoolConfig) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [PoolConfig]).

%% ============================================================================
%% Behavior callbacks
%% ============================================================================

init([PoolConfig]) ->
    % start event manager
    {ok, _Pid} = gen_event:start_link({local, httpclient_service_eventman}),
    PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
        PoolArgs = [{name, {local, Name}},
                    {worker_module, httpclient_service}] ++ SizeArgs,
        poolboy:child_spec(Name, PoolArgs, WorkerArgs)
    end, PoolConfig),
    MaxRestart = 1,
    MaxTime = 3600,
    {ok, {{one_for_one, MaxRestart, MaxTime}, PoolSpecs}}.
