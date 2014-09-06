-module(httpclient_sup).

-behaviour(supervisor).

%% User functions
-export([start_link/0]).
-export([start_pool/2]).

%% Behavior callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Module, Args, Type), {Id, {Module, start_link, Args},
        permanent, 5000, Type, [Module]}).

%% ===================================================================
%% User functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Behavior callbacks
%% ===================================================================

init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.

start_pool(Connections, PoolConfig) ->
    LoginSup = ?CHILD(httpclient_login_sup, httpclient_login_sup,
        [Connections], supervisor),
    ServiceSup = ?CHILD(httpclient_service_sup, httpclient_service_sup,
        [PoolConfig], supervisor),
    {ok, _LoginSupPid} = supervisor:start_child(?MODULE, LoginSup),
    {ok, _ServiceSupPid} = supervisor:start_child(?MODULE, ServiceSup),
    ok.
