%% http request record
-record (httpclient_http, {method, protocol, host, port, path, body, headers}).
