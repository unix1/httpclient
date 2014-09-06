-module(httpclient_conn).

%% connection record
-record (httpclient_conn, {protocol, host, port, user, pass, token, pool,
                           http_backend, login_handler, service_handler}).

%% User functions
-export([new/9]).
-export([get_backend/1]).
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
-export([set_token/2]).

%% ============================================================================
%% User functions
%% ============================================================================

new(Protocol, Host, Port, User, Pass, Pool, HttpBackend, LoginHandler,
    ServiceHandler) ->
    #httpclient_conn{protocol = Protocol, host = Host, port = Port,
        user = User, pass = Pass, pool = Pool, http_backend = HttpBackend,
        login_handler = LoginHandler, service_handler = ServiceHandler}.

get_backend(Conn) ->
    Conn#httpclient_conn.http_backend.

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

set_token(Conn, Token) ->
    Conn#httpclient_conn{token = Token}.
