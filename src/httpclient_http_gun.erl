-module(httpclient_http_gun).

-include("httpclient_http.hrl").

-behaviour(httpclient_http_handler).

%% Behavior callbacks
-export([init/4]).
-export([terminate/1]).
-export([send_request/2]).

-record (state, {pid, mref}).

%% ============================================================================
%% Behavior callbacks
%% ============================================================================

init(Protocol, Host, Port, undefined) ->
    init(Protocol, Host, Port, #{});
init(Protocol, Host, Port, Options) ->
    Transport = if Protocol =:= "https" -> tls; true -> tcp end,
    Opts = Options#{protocols => [http], transport => Transport},
    {ok, Pid} = gun:open(Host, Port, Opts),
    link(Pid),
    S = #state{pid = Pid},
    {ok, S}.

send_request(#state{pid = Pid}, #httpclient_http{method = get,
                                                 protocol = _Protocol,
                                                 host = _Host,
                                                 port = _Port,
                                                 path = Path,
                                                 body = <<>>,
                                                 headers = Headers}) ->
    StreamRef = gun:get(Pid, Path, Headers),
    {ok, Status, ResponseHeaders, ResponseBody} = await(Pid, StreamRef),
    gun:flush(Pid),
    {ok, Status, ResponseHeaders, ResponseBody};
send_request(#state{pid = Pid}, #httpclient_http{method = Method,
                                                 protocol = _Protocol,
                                                 host = _Host,
                                                 port = _Port,
                                                 path = Path,
                                                 body = Body,
                                                 headers = Headers}) ->
    StreamRef = gun:Method(Pid, Path, Headers, Body),
    {ok, Status, ResponseHeaders, ResponseBody} = await(Pid, StreamRef),
    gun:flush(Pid),
    {ok, Status, ResponseHeaders, ResponseBody}.

await(Pid, StreamRef) ->
    case gun:await(Pid, StreamRef) of
        {response, fin, Status, Headers} ->
            {ok, Status, Headers, <<>>};
        {response, nofin, Status, Headers} ->
            {ok, Body} = gun:await_body(Pid, StreamRef),
            {ok, Status, Headers, Body}
    end.

terminate(S) ->
    gun:shutdown(S#state.pid),
    ok.
