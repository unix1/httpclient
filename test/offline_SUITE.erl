-module(offline_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([httpclient_conn_new/1]).
-export([httpclient_conn_set/1]).
-export([httpclient_req_new3/1]).
-export([httpclient_req_new4/1]).
-export([httpclient_req_new5/1]).
-export([httpclient_req_add_header/1]).
-export([httpclient_req_set/1]).
-export([httpclient_http_init/1]).
-export([httpclient_http_request_bodyparams/1]).
-export([httpclient_http_request_nobody/1]).

-define(HOST, "localhost").
-define(PROTOCOL, "https").
-define(PORT, 443).
-define(USER, <<>>).
-define(PASS, <<>>).
-define(POOL, default).
-define(BACKEND_HTTP, httpclient_http_mock).
-define(BACKEND_LOGIN, dummy_login).
-define(BACKEND_SERVICE, dummy_service).

%% ============================================================================
%% ct functions
%% ============================================================================

all() ->
    [
      httpclient_conn_new,
      httpclient_conn_set,
      httpclient_req_new3,
      httpclient_req_new4,
      httpclient_req_new5,
      httpclient_req_add_header,
      httpclient_req_set,
      httpclient_http_init,
      httpclient_http_request_bodyparams,
      httpclient_http_request_nobody
    ].

init_per_suite(Config) ->
    ok = application:load(httpclient),
    ok = httpclient:start(),
    Config.

end_per_suite(_) ->
    application:stop(httpclient),
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%% ============================================================================
%% Tests
%% ============================================================================

httpclient_conn_new(_) ->
    Conn = httpclient_http_help_mock_conn(),
    ?PROTOCOL = httpclient_conn:get_protocol(Conn),
    ?HOST = httpclient_conn:get_host(Conn),
    ?PORT = httpclient_conn:get_port(Conn),
    ?USER = httpclient_conn:get_user(Conn),
    ?PASS = httpclient_conn:get_pass(Conn),
    ?POOL = httpclient_conn:get_pool(Conn),
    ?BACKEND_HTTP = httpclient_conn:get_backend(Conn),
    ?BACKEND_LOGIN = httpclient_conn:get_login_handler(Conn),
    ?BACKEND_SERVICE = httpclient_conn:get_service_handler(Conn).

httpclient_conn_set(_) ->
    Conn = httpclient_http_help_mock_conn(),
    {Backend, Token} = {"test_backend", "test_token"},
    Conn1 = httpclient_conn:set_token(Conn, Token),
    Conn2 = httpclient_conn:set_backend(Conn1, Backend),
    Backend = httpclient_conn:get_backend(Conn2),
    Token = httpclient_conn:get_token(Conn2).

httpclient_req_new3(_) ->
    {Method, Headers, Path} = {get, [<<"foo">>, <<"bar">>], <<"/foo/bar">>},
    Req = httpclient_req:new(Method, Headers, Path),
    Method = httpclient_req:get_method(Req),
    Headers = httpclient_req:get_headers(Req),
    Path = httpclient_req:get_path(Req).

httpclient_req_new4(_) ->
    {Method, Headers, Path, Params} =
        {get, [<<"foo">>, <<"bar">>], <<"/foo/bar">>,
         [{<<"param1">>, <<"value1">>}, {<<"param2">>, <<"value2">>}]},
    Req = httpclient_req:new(Method, Headers, Path, Params),
    Method = httpclient_req:get_method(Req),
    Headers = httpclient_req:get_headers(Req),
    Path = httpclient_req:get_path(Req),
    Params = httpclient_req:get_params(Req).

httpclient_req_new5(_) ->
    {Method, Headers, Path, Params, Body} =
        {get, [<<"foo">>, <<"bar">>], <<"/foo/bar">>,
         [{<<"param1">>, <<"value1">>}, {<<"param2">>, <<"value2">>}],
         <<"test body">>},
    Req = httpclient_req:new(Method, Headers, Path, Params, Body),
    Method = httpclient_req:get_method(Req),
    Headers = httpclient_req:get_headers(Req),
    Path = httpclient_req:get_path(Req),
    Params = httpclient_req:get_params(Req),
    Body = httpclient_req:get_body(Req).

httpclient_req_add_header(_) ->
    {Method, [H1, H2], Path} = {get, [<<"foo">>, <<"bar">>], <<"/foo/bar">>},
    Req1 = httpclient_req:new(Method, [H1, H2], Path),
    H3 = <<"baz">>,
    Req2 = httpclient_req:add_header(Req1, H3),
    Headers = httpclient_req:get_headers(Req2),
    true = lists:member(H1, Headers),
    true = lists:member(H2, Headers),
    true = lists:member(H3, Headers).

httpclient_req_set(_) ->
    {Method, Headers, Path, Params, Body} =
        {post, [<<"foo">>, <<"bar">>], <<"/foo/bar">>,
         [{<<"param1">>, <<"value1">>}, {<<"param2">>, <<"value2">>}],
         <<"body original">>},
    Req = httpclient_req:new(Method, Headers, Path, Params, Body),
    {BodyNew, MethodNew, PathNew, ParamsNew} =
        {<<"body new">>, post, <<"/foo/baz">>,
         [{<<"p1">>, <<"v1">>}, {<<"p2">>, <<"v2">>}]},
    Req1 = httpclient_req:set_body(Req, BodyNew),
    Req2 = httpclient_req:set_method(Req1, MethodNew),
    Req3 = httpclient_req:set_path(Req2, PathNew),
    Req4 = httpclient_req:set_params(Req3, ParamsNew),
    BodyNew = httpclient_req:get_body(Req4),
    MethodNew = httpclient_req:get_method(Req4),
    PathNew = httpclient_req:get_path(Req4),
    ParamsNew = httpclient_req:get_params(Req4).

httpclient_http_init(_) ->
    Conn = httpclient_http_help_mock_conn(),
    {ok, [?PROTOCOL, ?HOST, ?PORT]} = httpclient_http:init(Conn).

httpclient_http_request_bodyparams(_) ->
    Conn = httpclient_http_help_mock_conn(),
    State = {arbitrary, "State"},
    {Method, Headers, Path, Params, Body} =
        {post, [<<"foo">>, <<"bar">>], <<"/return/request">>,
         [{<<"param1">>, <<"value1">>}, {<<"param2">>, <<"value2">>}],
         <<"body original">>},
    PathExpected = <<"/return/request?param1=value1&param2=value2">>,
    Req = httpclient_req:new(Method, Headers, Path, Params, Body),
    % verify response matches request
    {ok, 200, Method, Headers, PathExpected, Body} =
        httpclient_http:request(Conn, State, Req).

httpclient_http_request_nobody(_) ->
    Conn = httpclient_http_help_mock_conn(),
    State = {arbitrary, "State"},
    {Method, Headers, Path, Params} =
        {post, [<<"foo">>, <<"bar">>], <<"/return/request">>,
         [{<<"param1">>, <<"value1">>}, {<<"param2">>, <<"value2">>}]},
    PathExpected = <<"/return/request?param1=value1&param2=value2">>,
    Req = httpclient_req:new(Method, Headers, Path, Params),
    % verify response matches request
    {ok, 200, Method, Headers, PathExpected, <<>>} =
        httpclient_http:request(Conn, State, Req).

%% ============================================================================
%% Internal functions
%% ============================================================================

httpclient_http_help_mock_conn() ->
    Config = [{protocol, ?PROTOCOL},
              {host, ?HOST},
              {port, ?PORT},
              {user, ?USER},
              {pass, ?PASS},
              {pool, ?POOL},
              {http_backend, ?BACKEND_HTTP},
              {login_handler, ?BACKEND_LOGIN},
              {service_handler, ?BACKEND_SERVICE}],
    httpclient_conn:new(Config).
