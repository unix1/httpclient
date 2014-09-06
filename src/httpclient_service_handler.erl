-module(httpclient_service_handler).

-callback get_request(Fun :: atom(), Args :: list(), Token :: term()) ->
    tuple('ok', Req :: term()) | tuple('error', Reason :: string()).
