-module(httpclient_http_handler).

-callback init(Protocol :: string(),
               Host :: string(),
               Port :: non_neg_integer(),
               Options :: list(tuple())) ->
    {'ok', State :: term()} | {'error', Reason :: string()}.

-callback terminate(Args :: list(term())) ->
    'ok'|{'error', Reason :: string()}.

-callback send_request(State :: term(),
                       Request :: term()) ->
    {'ok', ResponseBody :: string()} | {'error', ErrorReason :: string()}.
