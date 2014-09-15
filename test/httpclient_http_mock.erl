-module(httpclient_http_mock).

-include("../src/httpclient_http.hrl").

-behaviour(httpclient_http_handler).

%% Behavior callbacks
-export([init/3]).
-export([send_request/2]).
-export([terminate/1]).

init(Protocol, Host, Port) ->
    S = [Protocol, Host, Port],
    {ok, S}.

send_request(_State, #httpclient_http{
        path = <<"/return/request?param1=value1&param2=value2">>} = Req) ->
    Status = 200,
    Headers = Req#httpclient_http.headers,
    Method = Req#httpclient_http.method,
    Body = Req#httpclient_http.body,
    Path = Req#httpclient_http.path,
    Body = Req#httpclient_http.body,
    {ok, Status, Method, Headers, Path, Body};
send_request(_State, #httpclient_http{path = <<"/return/200">>}) ->
    Status = 200,
    Headers = [{<<"foo">>,<<"bar">>}],
    Body = <<"test response">>,
    {ok, Status, Headers, Body};
send_request(_State, #httpclient_http{path = <<"/return/500">>}) ->
    Status = 500,
    Headers = [{<<"foo">>,<<"bar">>}],
    Body = <<"test response">>,
    {ok, Status, Headers, Body}.

terminate(_State) ->
    ok.
