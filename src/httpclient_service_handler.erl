-module(httpclient_service_handler).

-callback get_request(Fun :: atom(), Args :: list(), Token :: term()) ->
    {'ok', Req :: term()} | {'error', Reason :: string()}.
