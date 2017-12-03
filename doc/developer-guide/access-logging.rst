.. _dev-access-logging:

Access Logging
=======

Zotonic does access logging via syslog. All web requests are logged and available for
inspection.

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