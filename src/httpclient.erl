-module(httpclient).

%% User functions
-export([start_pool/2]).
-export([stop_pool/1]).
-export([request/2, request/3]).

%% ============================================================================
%% User functions
%% ============================================================================

start_pool(ConnectionsConfig, PoolConfig) ->
    F = fun({Name, Config}) ->
        {Name, httpclient_conn:new(Config)}
    end,
    Connections = lists:map(F, ConnectionsConfig),
    ok = httpclient_sup:start_pool(Connections, PoolConfig).

stop_pool(_Name) ->
    ok.

request(_Method, _Req) ->
    ok.

request(_Method, _Req, {backend, _Backend}) ->
    ok;
request(_Method, _Req, {pool, _Pool}) ->
    ok.
