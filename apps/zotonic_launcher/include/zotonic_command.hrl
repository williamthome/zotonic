%%%-------------------------------------------------------------------
%%% @author Blaise
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Dec 2017 6:52 PM
%%%-------------------------------------------------------------------
-author("Blaise").

-define(DEFAULT_NODENAME, "zotonic").
-define(DEFAULT_NODENAME_TEST, "zotonic001_testsandbox").

-define(MAXWAIT, 30).

-record(cmd_option, {
    name           :: binary(),
    description    :: binary(),
    arg            :: binary(),
    values  = []   :: [binary()],
    default = <<>> :: binary()
}).

-record(cmd_info, {
    name           :: binary(),
    description    :: binary(),
    run            :: binary(),
    options = []   :: [#cmd_option{}]
}).
