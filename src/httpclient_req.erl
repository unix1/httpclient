-module(httpclient_req).

-include("httpclient_req.hrl").

%% User functions
-export([new/3, new/4, new/5]).
-export([add_header/2]).
-export([get_body/1]).
-export([get_headers/1]).
-export([get_method/1]).
-export([get_path/1]).
-export([get_params/1]).
-export([set_body/2]).
-export([set_method/2]).
-export([set_params/2]).
-export([set_path/2]).

new(Method, Headers, Path) ->
    #httpclient_req{method = Method, headers = Headers, path = Path}.

new(Method, Headers, Path, Params) ->
    #httpclient_req{method = Method, headers = Headers, path = Path,
                    params = Params}.

new(Method, Headers, Path, Params, Body) ->
    #httpclient_req{method = Method, headers = Headers, path = Path,
                    params = Params, body = Body}.

add_header(Req, Header) ->
    Req#httpclient_req{headers = [Header|Req#httpclient_req.headers]}.

get_body(Req) ->
    Req#httpclient_req.body.

get_headers(Req) ->
    Req#httpclient_req.headers.

get_method(Req) ->
    Req#httpclient_req.method.

get_params(Req) ->
    Req#httpclient_req.params.

get_path(Req) ->
    Req#httpclient_req.path.

set_body(Req, Body) ->
    Req#httpclient_req{body = Body}.

set_method(Req, Method) ->
    Req#httpclient_req{method = Method}.

set_params(Req, Params) ->
    Req#httpclient_req{params = Params}.

set_path(Req, Path) ->
    Req#httpclient_req{path = Path}.
