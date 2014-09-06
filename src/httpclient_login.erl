-module(httpclient_login).

-behaviour(gen_server).

%% Behavior callbacks
-export([start_link/2]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% User functions
-export([login/1]).
-export([get_token/1]).
-export([get_connection/1]).
-export([get_pool/1]).

%% State record
-record (state, {connection, http_state}).

%% ============================================================================
%% Supervision functions
%% ============================================================================

%% start with locally registered name, supplied from supervisor
start_link(Name, Conn) when is_atom(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Conn], []).

%% init, set state
init([Conn]) ->
    {ok, HttpState} = httpclient_http:init(Conn),
    S = #state{connection = Conn, http_state = HttpState},
    {ok, S}.

%% ============================================================================
%% User functions
%% ============================================================================

login(Name) ->
    gen_server:call(Name, {login}).

get_token(Name) ->
    gen_server:call(Name, {get_token}).

get_connection(Name) ->
    gen_server:call(Name, {get_connection}).

% TODO see if this can be done without sending a message to gen_server
% because this is on critical execution path
get_pool(Name) ->
    gen_server:call(Name, {get_pool}).

%% ============================================================================
%% Server functions
%% ============================================================================

handle_call({login}, _From, S) ->
    LoginHandler = httpclient_conn:get_login_handler(S#state.connection),
    case LoginHandler:login(S#state.connection, S#state.http_state) of
        {ok, Token} ->
            gen_event:notify(httpclient_service_eventman, {token, Token}),
            NewState = S#state{connection =
                httpclient_conn:set_token(S#state.connection, Token)},
            {reply, ok, NewState};
        {error, Reason} ->
            {stop, error, Reason, S};
        _Else ->
            {stop, error, "Unknown error", S}
    end;
handle_call({get_token}, _From, S) ->
    {reply, httpclient_conn:get_token(S#state.connection), S};
handle_call({get_connection}, _From, S) ->
    {reply, S#state.connection, S};
handle_call({get_pool}, _From, S) ->
    {reply, httpclient_conn:get_pool(S#state.connection), S}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, S) ->
    ok = httpclient_http:terminate(S#state.connection, S#state.http_state).

code_change(_OldVersion, State, _Extra) -> {ok, State}.
