-module(httpclient_conn).

%% connection record
-record (httpclient_conn, {protocol, host, port, user, pass, token, pool,
                           http_backend, http_backend_options, login_handler,
                           service_handler}).

%% User functions
-export([new/1]).
-export([get_backend/1]).
-export([get_backend_options/1]).
-export([get_host/1]).
-export([get_login_handler/1]).
-export([get_pass/1]).
-export([get_pool/1]).
-export([get_port/1]).
-export([get_protocol/1]).
-export([get_service_handler/1]).
-export([get_token/1]).
-export([get_user/1]).
-export([set_backend/2]).
-export([set_backend_options/2]).
-export([set_token/2]).

%% ============================================================================
%% User functions
%% ============================================================================
new(Config) ->
    F = fun({protocol, Value}, Conn) -> Conn#httpclient_conn{protocol = Value};
           ({host, Value}, Conn) -> Conn#httpclient_conn{host = Value};
           ({port, Value}, Conn) -> Conn#httpclient_conn{port = Value};
           ({user, Value}, Conn) -> Conn#httpclient_conn{user = Value};
           ({pass, Value}, Conn) -> Conn#httpclient_conn{pass = Value};
           ({pool, Value}, Conn) -> Conn#httpclient_conn{pool = Value};
           ({http_backend, Value}, Conn) -> Conn#httpclient_conn{http_backend = Value};
           ({http_backend_options, Value}, Conn) -> Conn#httpclient_conn{http_backend_options = Value};
           ({login_handler, Value}, Conn) -> Conn#httpclient_conn{login_handler = Value};
           ({service_handler, Value}, Conn) -> Conn#httpclient_conn{service_handler = Value} end,
    lists:foldl(F, #httpclient_conn{}, Config).

get_backend(Conn) ->
    Conn#httpclient_conn.http_backend.

get_backend_options(Conn) ->
    Conn#httpclient_conn.http_backend_options.

get_host(Conn) ->
    Conn#httpclient_conn.host.

get_login_handler(Conn) ->
    Conn#httpclient_conn.login_handler.

get_pass(Conn) ->
    Conn#httpclient_conn.pass.

get_pool(Conn) ->
    Conn#httpclient_conn.pool.

get_port(Conn) ->
    Conn#httpclient_conn.port.

get_protocol(Conn) ->
    Conn#httpclient_conn.protocol.

get_service_handler(Conn) ->
    Conn#httpclient_conn.service_handler.

get_token(Conn) ->
    Conn#httpclient_conn.token.

get_user(Conn) ->
    Conn#httpclient_conn.user.

set_backend(Conn, Backend) ->
    Conn#httpclient_conn{http_backend = Backend}.

set_backend_options(Conn, Options) ->
    Conn#httpclient_conn{http_backend_options = Options}.

set_token(Conn, Token) ->
    Conn#httpclient_conn{token = Token}.
