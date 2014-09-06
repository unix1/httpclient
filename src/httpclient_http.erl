-module(httpclient_http).

-include("httpclient_http.hrl").
-include("httpclient_req.hrl").

%% User functions
-export([init/1]).
-export([request/3]).
-export([terminate/2]).

%% ============================================================================
%% User functions
%% ============================================================================

init(Conn) ->
    Backend = httpclient_conn:get_backend(Conn),
    Protocol = httpclient_conn:get_protocol(Conn),
    Host = httpclient_conn:get_host(Conn),
    Port = httpclient_conn:get_port(Conn),
    {ok, _State} = Backend:init(Protocol, Host, Port).

request(Conn, State, Req) ->
    Request = combine_request(Conn, Req),
    Backend = httpclient_conn:get_backend(Conn),
    send_request(Backend, State, Request).

terminate(C, State) ->
    Backend = httpclient_conn:get_backend(C),
    ok = Backend:terminate(State).

%% ============================================================================
%% Internal functions
%% ============================================================================

combine_request(Conn, Req) ->
    #httpclient_http{method = httpclient_req:get_method(Req),
                     protocol = httpclient_conn:get_protocol(Conn),
                     host = httpclient_conn:get_host(Conn),
                     port = httpclient_conn:get_port(Conn),
                     path = iolist_to_binary(
                                get_path(httpclient_req:get_path(Req),
                                         httpclient_req:get_params(Req))),
                     body = iolist_to_binary(
                                get_body(httpclient_req:get_body(Req))),
                     headers = httpclient_req:get_headers(Req)}.

get_path(BasePath, Params) ->
    case Params of
        [] ->
            BasePath;
        _Else ->
            build_query_string(Params, [BasePath, <<"?">>])
    end.

get_body(undefined) ->
    <<>>;
get_body(BodyParams) when is_list(BodyParams) ->
    build_query_string(BodyParams);
get_body(Body) when is_binary(Body) ->
    Body.

build_query_string(Params) ->
    build_query_string(Params, <<>>).

build_query_string(undefined, Acc) ->
    Acc;
build_query_string([], Acc) ->
    Acc;
build_query_string([{Name, Value}|Rest], <<>>) ->
    build_query_string(
        Rest,
        [urlencode(Name), <<"=">>, urlencode(Value)]);
build_query_string([{Name, Value}|Rest], Acc) ->
    build_query_string(
        Rest,
        [Acc, <<"&">>, urlencode(Name), <<"=">>, urlencode(Value)]).

urlencode(Input) when is_binary(Input) ->
    list_to_binary(http_uri:encode(binary_to_list(Input))).

send_request(Backend, State, Request) ->
    Backend:send_request(State, Request).
