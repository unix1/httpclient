-module(httpclient_http_gun).

-include("httpclient_http.hrl").

-behaviour(httpclient_http_handler).

%% Behavior callbacks
-export([init/3]).
-export([terminate/1]).
-export([send_request/2]).

-record (state, {pid, mref}).

%% ============================================================================
%% Behavior callbacks
%% ============================================================================

init(Protocol, Host, Port) ->
    Type = if Protocol =:= "https" -> ssl; true -> tcp end,
    Opts = [{type, Type}, {keepalive, 500}, {retry_timeout, 500}],
    {ok, Pid} = gun:open(Host, Port, Opts),
    Mref = monitor(process, Pid),
    S = #state{pid = Pid, mref = Mref},
    {ok, S}.

send_request(#state{pid = Pid, mref = Mref}, #httpclient_http{method = get,
                                                   protocol = _Protocol,
                                                   host = _Host,
                                                   port = _Port,
                                                   path = Path,
                                                   body = <<>>,
                                                   headers = Headers}) ->
    ok = receive_down(Pid, Mref),
    StreamRef = gun:get(Pid, Path, Headers),
    {ok, _Status, _Headers, _Body} = receive_data(Pid, Mref, StreamRef);
send_request(#state{pid = Pid, mref = Mref}, #httpclient_http{method = Method,
                                                   protocol = _Protocol,
                                                   host = _Host,
                                                   port = _Port,
                                                   path = Path,
                                                   body = Body,
                                                   headers = Headers}) ->
    ok = receive_down(Pid, Mref),
    StreamRef = gun:Method(Pid, Path, Headers, Body),
    {ok, _Status, _Headers, _Body} = receive_data(Pid, Mref, StreamRef).

receive_data(Pid, Mref, StreamRef) ->
    receive
        {'DOWN', Mref, process, Pid, Reason} ->
            {error, incomplete, Reason};
        {gun_response, Pid, StreamRef, fin, Status, Headers} ->
            {ok, Status, Headers, <<>>};
        {gun_response, Pid, StreamRef, nofin, Status, Headers} ->
            {ok, ResponseBody} = receive_data_loop(Pid, Mref, StreamRef, <<>>),
            {ok, Status, Headers, ResponseBody};
        Other ->
            io:format("gun process got message: ~s~n", [Other])
    after 5000 ->
        {error, timeout}
    end.

receive_data_loop(Pid, Mref, StreamRef, Acc) ->
    receive
        {'DOWN', Mref, process, Pid, Reason} ->
            {error, incomplete, Reason};
        {gun_data, Pid, StreamRef, nofin, Data} ->
            receive_data_loop(Pid, Mref, StreamRef, [Acc, Data]);
        {gun_data, Pid, StreamRef, fin, Data} ->
            {ok, iolist_to_binary([Acc, Data])};
        Other ->
            io:format("gun process got message: ~s~n", [Other])
    after 5000 ->
        {error, timeout}
    end.

receive_down(Pid, Mref) ->
    receive
        {'DOWN', Mref, process, Pid, Reason} ->
            {error, Reason};
        Other ->
            {error, Other}
    after 0 ->
        ok
    end.

terminate(S) ->
    gun:shutdown(S#state.pid),
    ok.
