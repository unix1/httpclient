httpclient Login Handler
========================

This chapter describes how to implement a login handler behavior of the
`httpclient` application. This behavior is where you specify application
specific authentication logic.

Behavior
--------

There is only one function to implement

````erlang
-callback login(Conn :: term(), HttpState :: term()) ->
    tuple('ok', Token :: term()) | tuple('error', Reason :: string()).
````

In short, you will get passed a Connection object and a HttpState object,
and the goal is to get back an `{ok, Token}` return where the `Token` is
a usable authentication information by each service pool worker.

Notes:
* to read the `Conn` argument, use the `httpclient_conn` module
* if you need to make an HTTP call, which is very likely, you are encouraged
    to create an `httpclient_req` and use `httpclient_http:request/3`
    function to which you would pass the `HttpState` value
* `HttpState` need only be used as above
* the use cases for this are OAuth2, OpenID, and variety of other similar
    custom protocols used for authentication

Logging in
----------

Once you have started a pool, logging in is easy: just call
`httpclient_login:login(default)` from your application, where `default` is
a connection tuple name defined in your connection configuration. This will
trigger a callback to your application-specific login implementation above.
