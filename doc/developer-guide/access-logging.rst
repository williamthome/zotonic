.. _dev-access-logging:

Access Logging
=======

Zotonic does access logging via syslog. All web requests are logged and available for
inspection.

Access Log Message Format
-------------

ip-address:port [Date] scheme host:zotonic_site "request" request-status response-code bytes-received/bytes-sent "referrer" "user-agent-string"

request-status:

N | Normal | The request was handled normally.
P | Protocol Upgrade | Protocol upgrade, usually websocket connections.
EI | Internal Error | Error caused in Cowboy or inside a stream handler.
EC | Connection Error | These kind of errors are usually caused by external factors, like buggy client software or software scanning for vulerabilities.
ER | Stream Error |
ES | Socket Error
S | Stop (HTTP 2)
* | Unknown

Examples

Normal Response

::1:49856 [03/Dec/2017:09:30:33 +0000] https localhost:8443:zotonic_site_status "GET /zotonic/status HTTP/1.1" N 200 0/3428 "http://localhost:8000/" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/604.3.5 (KHTML, like Gecko) Version/11.0.1 Safari/604.3.5"

Protocol Upgrade

::1:49862 [03/Dec/2017:09:30:33 +0000] https localhost:8443:zotonic_site_status "GET /websocket HTTP/1.1" P 101 0/0 "-" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/604.3.5 (KHTML, like Gecko) Version/11.0.1 Safari/604.3.5"

Errors
-------------

ER | Stream Error



% @doc Format the request reason.

fmt_reason(socket_error) -> <<"ES">>;
fmt_reason(stream_error) -> <<"ER">>;
fmt_reason(stop) -> $S;


Connection Errors

These kind of errors are usually caused by external factors, like buggy client software or
software scanning for vulerabilities.

127.0.0.1:49886 [03/Dec/2017:09:56:42 +0000] - -:zotonic_site_status "- - -" EC 505 0/0 "-" "-"

Socket Errors.

These kind of errors are usually caused by external factors. Usually by sockets which are closed
abruptly in the middle of handling a request.

Internal Errors

These kind of errors happen inside cowboy, or stream handlers.

fmt_reason(internal_error) -> <<"EI">>;
[03/Dec/2017:09:46:46 +0000] http localhost:8000:zotonic_site_status "GET / HTTP/1.1" EI - 0/0 "-" "curl/7.54.0"

Unkown Errors

[03/Dec/2017:09:46:46 +0000] http localhost:8000:zotonic_site_status "GET / HTTP/1.1" * - 0/0 "-" "curl/7.54.0"


Configuration
-------------

In :ref:`zotonic-config` file, change the ``syslog_`` section to configure syslog
settings. See the `Syslog`_ for more information.


. code-block:: erlang
    :caption: zotonic.config

     {syslog_ident, "zotonic"},
     {syslog_opts, [ndelay]},
     {syslog_facility, local0},
     {syslog_level, info},

.. _Syslog: https://github.com/Vagabond/erlang-syslog