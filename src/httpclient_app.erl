-module(httpclient_app).

-behaviour(application).

%% Behavior callbacks
-export([start/2]).
-export([stop/1]).

%% ============================================================================
%% Behavior callbacks
%% ============================================================================

start(_StartType, _StartArgs) ->
    httpclient_sup:start_link().

stop(_State) ->
    ok.

