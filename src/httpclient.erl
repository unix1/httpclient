-module(httpclient).

-behaviour(application).

%% Behavior callbacks
-export([start/2]).
-export([stop/1]).

%% User functions
-export([start/0]).
-export([stop/0]).
-export([start_pool/2]).
-export([stop_pool/1]).
-export([request/2, request/3]).

%% ============================================================================
%% Behavior callbacks
%% ============================================================================

start(_StartType, _StartArgs) ->
    httpclient_sup:start_link().

stop(_State) ->
    ok.

%% ============================================================================
%% User functions
%% ============================================================================

start() ->
    ok = application:ensure_started(crypto),
    ok = application:ensure_started(cowlib),
    ok = application:ensure_started(asn1),
    ok = application:ensure_started(public_key),
    ok = application:ensure_started(ssl),
    ok = application:ensure_started(inets),
    ok = application:ensure_started(poolboy),
    ok = application:ensure_started(gun),
    ok = application:ensure_started(httpclient),
    ok.

stop() ->
    ok = application:stop(httpclient),
    ok = application:stop(gun),
    ok = application:stop(poolboy),
    ok = application:stop(inets),
    ok = application:stop(ssl),
    ok = application:stop(public_key),
    ok = application:stop(asn1),
    ok = application:stop(cowlib),
    ok = application:stop(crypto),
    ok.

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
