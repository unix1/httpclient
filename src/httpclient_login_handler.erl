-module(httpclient_login_handler).

-callback login(Conn :: term(), HttpState :: term()) ->
    tuple('ok', Token :: term()) | tuple('error', Reason :: string()).
