-module(httpclient_http_httpc).

-include("httpclient_http.hrl").

-behaviour(httpclient_http_handler).

%% Behavior callbacks
-export([init/3]).
-export([terminate/1]).
-export([send_request/2]).

%% ============================================================================
%% Behavior callbacks
%% ============================================================================

init(_Protocol, _Host, _Port) ->
    {ok, 0}.

send_request(_State, #httpclient_http{method = Method,
                                        protocol = Protocol,
                                        host = Host,
                                        port = Port,
                                        path = Path,
                                        body = "",
                                        headers = Headers}) ->
    HTTPOptions = [{relaxed, true}],
    Options = [{body_format, binary}],
    Uri = get_uri(Protocol, Host, Port, Path),
    {ok, {{_Version, Status, _ReasonPhrase}, ResponseHeaders, ResponseBody}} =
        httpc:request(Method, {Uri, Headers}, HTTPOptions, Options),
    {ok, Status, ResponseHeaders, ResponseBody};
send_request(_State, #httpclient_http{method = Method,
                                        protocol = Protocol,
                                        host = Host,
                                        port = Port,
                                        path = Path,
                                        body = Body,
                                        headers = Headers}) ->
    HTTPOptions = [{relaxed, true}],
    Options = [{body_format, binary}],
    Uri = get_uri(Protocol, Host, Port, Path),
    Type = case proplists:get_value("content-type", Headers) of
        undefined -> "";
        Value -> Value
    end,
    {ok, {{_Version, Status, _ReasonPhrase}, ResponseHeaders, ResponseBody}} =
        httpc:request(Method, {Uri, Headers, Type, Body}, HTTPOptions, Options),
    {ok, Status, ResponseHeaders, ResponseBody}.

terminate(_State) ->
    ok.

%% ============================================================================
%% Internal functions
%% ============================================================================

get_uri(Protocol, Host, Port, Path) ->
    Protocol ++ "://" ++ Host ++ ":" ++ integer_to_list(Port) ++ Path.
