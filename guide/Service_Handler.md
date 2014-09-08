httpclient Service Handler
==========================

This chapter describes how to implement a service handler for `httpclient`
application. This behavior is where you specify how your application
specific function and arguments translate to a `httpclient_req`.

Example
-------

Below is an example of a simple module:

````erlang
-module(my_app_service).
-behaviour(httpclient_service_handler).

% User functions
-export([post_comment/3]).

% Behavior callbacks
-export([get_request/1]).

post_comment(ConnName, User, Comment) ->
    {ok, 200, ResponseHeaders, ResponseBody} =
        httpclient_service:request(ConnName, {post_comment, [User, Comment]}),

get_request(post_comment, [User, Comment], Token) ->
    Headers = [{<<"Auth-Header">>, Token},
        {<<"content-type">>, <<"application/x-www-form-urlencoded">>}],
    Path = [<<"/users/comments">>],
    Params = [{<<"user">>, User}],
    Body = [{<<"comment">>, Comment}],
    Req = httpclient_req:new(post, Headers, Path, Params, Body),
    {ok, Req}.
````

There are 2 things happening in this module:

* `post_comment` is a function that you would call in your application and
    pass it arbitrary arguments; this function should execute
    `httpclient_service:request/2` function passing the connection name and
    the tuple about this request containing the name and the arguments
* `get_request/3` is required by the behavior and is used during the HTTP
    request process to get the `httpclient_req`; given the `post_comment`
    name and the list of arguments, you must translate it into a simple
    HTTP relative request and return it with `{ok, Req}`

Note that the `httpclient_req` is a relative request. This is because you
already provided the server connection information in your configuration.
