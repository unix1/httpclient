-module(httpclient_service).

-behaviour(gen_server).

%% Behavior callbacks
-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% User functions
-export([request/2]).
-export([update_connection_token/2]).

%% State record
-record (state, {connection, event_handler_id, http_state}).

%% ============================================================================
%% Supervision functions
%% ============================================================================
%% start without name
start_link(Config) ->
    gen_server:start_link(?MODULE, [Config], []).

%% gen_server init, set state
init([Config]) ->
    % add event handler
    HandlerId = {httpclient_service_event, make_ref()},
    gen_event:add_handler(httpclient_service_eventman, HandlerId, [self()]),
    ConnectionName = proplists:get_value(connection, Config),
    HttpBackend = proplists:get_value(http_backend, Config),
    HttpBackendOptions = proplists:get_value(http_backend_options, Config),
    Conn1 = httpclient_login:get_connection(ConnectionName),
    % override default connection http backend from service pool configuration
    Conn2 = httpclient_conn:set_backend(Conn1, HttpBackend),
    Conn3 = httpclient_conn:set_backend_options(Conn2, HttpBackendOptions),
    {ok, HttpState} = httpclient_http:init(Conn3),
    S = #state{connection = Conn2, event_handler_id = HandlerId,
        http_state = HttpState},
    {ok, S}.

%% ============================================================================
%% User functions
%% ============================================================================

request(ConnName, {Fun, Args}) ->
    Pool = httpclient_login:get_pool(ConnName),
    Worker = poolboy:checkout(Pool),
    Result = gen_server:call(Worker, {request, Fun, Args}),
    poolboy:checkin(Pool, Worker),
    Result.

update_connection_token(Pid, Token) ->
    gen_server:call(Pid, {update_token, Token}).

%% ============================================================================
%% Server functions
%% ============================================================================

handle_call({update_token, Token}, _From, S) ->
    Conn = httpclient_conn:set_token(S#state.connection, Token),
    {reply, ok, S#state{connection = Conn}};
handle_call({request, Fun, Args}, _From, S) ->
    Conn = S#state.connection,
    HttpState = S#state.http_state,
    ServiceHandler = httpclient_conn:get_service_handler(Conn),
    Token = httpclient_conn:get_token(Conn),
    {ok, Req} = ServiceHandler:get_request(Fun, Args, Token),
    {ok, R, H, B} = httpclient_http:request(Conn, HttpState, Req),
    {reply, {ok, R, H, B}, S}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, S) ->
    gen_event:delete_handler(httpclient_service_eventman,
        S#state.event_handler_id, []),
    ok = httpclient_http:terminate(S#state.connection, S#state.http_state).

code_change(_OldVersion, State, _Extra) -> {ok, State}.
