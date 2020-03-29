-module(httpclient_login_handler).

-callback login(Conn :: term(), HttpState :: term()) ->
    {'ok', Token :: term()} | {'error', Reason :: string()}.
