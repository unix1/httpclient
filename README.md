httpclient
==========

HTTP client wrapper with authentication and pooling services.

Summary
-------

`httpclient` provides an OTP application wrapper around existing low level
HTTP clients. In addition, it provides the following benefits:

* a configurable service pool for HTTP client workers
* an authentication service that automatically sends authentication info/tokens
    to client workers via gen_event
* HTTP low level backend implemented via a behavior - i.e. other backends can
    be added

On the other side, you only need to provide 3 things that are specific to your
application:

* configuration for your authentication and pooling needs
* implementation of your authentication that returns authentication info, e.g.
    a token, implemented via a login behavior
* translation of your application functions and arguments into a request
    record via a service behavior; these requests will get executed by the
    workers in specified pool

Notes
-----

* currently [gun](https://github.com/extend/gun) HTTP client is supported.
`httpc` code is there but it is untested with latest revisions and may be
buggy.
