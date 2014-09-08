Configuration
=============

You typically should keep the configuration in your application. This specifies

* your HTTP server connection information
* your authentication details
* your HTTP client pooling preferences

Example
-------

Below is an example of what you would normally keep in your application:

````erlang
[{my_app,
  [{connections,
    [{default,
      [{protocol, "https"},
       {host, "localhost"},
       {port, 443},
       {user, <<"my_user">>},
       {pass, <<"my_pass">>},
       {pool, my_pool},                     % service pool for this connection
       {http_backend, httpclient_http_gun}, % backend for login service
       {login_handler, my_app_login},       % authentication implementation
       {service_handler, my_app_service}    % service implementation
       ]}]},
     {service_pools,
      [{my_pool,
        [ % size args
          {size, 10},         % max pool size
          {max_overflow, 0}], % max # of workers created if pool is empty
        [ % worker args
          {connection, default}, % specifies which connection this pool maps to
          {http_backend, httpclient_http_gun} % backend for service workers
        ]}]}
  ]
}].
````
There are 2 tuples: `connections` and `service_pools`.

`connections` tuple contains a list of one or more connections, each of which
holds the following:
* configuration for the HTTP server, including the authentication information:
    protocol, host, port, username and password should be self-explanatory and
    will get passed back to you in your login implementation
* `pool`: reference to which service pool it is associated with so it can broadcast
    the authentication token to all pool members
* `http_backend`: what HTTP client to use; see note about what HTTP clients are
    currently available
* `login_handler`: name of the login handler of your application, in the above
    example `my_app_login`; this module should implement the
    `httpclient_login_handler` behavior and the specified `login` function;
    see more on this in the [Login Handler](Login_Handler.md) section
* `service_handler`: name of the module in your application implementing
    `httpclient_service_handler` behavior; see more on this in the
    [Service Handler](Service_Handler.md) section

`service_pools` tuple contains a list of one or more service pool
configurations. There are 2 lists in each pool tuple:

* size arguments: these currently closely mirror
    [poolboy](https://github.com/devinus/poolboy) arguments
* worker arguments: these get passed as arguments to the pool workers,
    specifying which connection they are part of, and which HTTP client backend
    they should be using; see note about what HTTP clients are currently
    available

Starting a Pool
---------------

Starting a pool is easy once you have the above configuration in your
application. Simply pass it to the `start_pool` function:

````erlang
httpclient:start_pool(Connections, Pools).
````

where `Connections` and `Pools` are `connections` and `service_pools` tuples
respectively from the above configuration.
