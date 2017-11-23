%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2015-2017 Maas-Maarten Zeeman
%% @doc An access logger which gets its log entries from an ets buffer and writes them to syslog

%% Copyright 2015-2017 Maas-Maarten Zeeman
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(z_access_syslog).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

%% Api

-export([start_link/0, start_link/4, log_access/1]).
-export([init/2, handle_value/4, handle_flush_done/2]).

-include_lib("zotonic.hrl").

-record(state, {
    priority,
    log,

    native_to_second = erlang:convert_time_unit(1, second, native),
    native_to_millisecond = erlang:convert_time_unit(1, millisecond, native)
}).

-record(log_data, {
    % Request
    peer,
    host,
    version,
    scheme,
    method,
    path,
    qs,
    request_body_length = 0,

    % Response
    reason,
    status,
    response_body_length = 0,

    % Common meta data
    user_agent,
    referer,

    start_time % The (corrected) start time of the request. Note this does not include receiving and handling headers.
}).

-define(FLUSH_INTERVAL, 1000).
-define(EPOCH, 62167219200).

%%
%% Api
%%

start_link() ->
    Ident = z_config:get(syslog_ident),
    Opts = z_config:get(syslog_opts),
    Facility = z_config:get(syslog_facility),
    Level = z_config:get(syslog_level),
    start_link(Ident, Opts, Facility, Level).

start_link(Ident, Opts, Facility, Level) ->
    z_buffered_worker:start_link(?MODULE, ?MODULE, [[Ident, Opts, Facility], Level]).

log_access(#{reason := normal, req := Req} = CowboyMetrics) ->
    LD1 = request_to_log_data(Req),

    #{resp_status := Status, req_body_length := ReqBodyLength, resp_body_length := RespBodyLength} = CowboyMetrics,
    #{req_start := StartTime} = CowboyMetrics,

    LD2 = LD1#log_data{
        reason = normal,
        status = Status,
        start_time = StartTime + erlang:time_offset(),
        request_body_length = ReqBodyLength,
        response_body_length = RespBodyLength
    },

    z_buffered_worker:push(?MODULE, LD2);

log_access(#{reason := switch_protocol, req := Req} = CowboyMetrics) ->
    LD1 = request_to_log_data(Req),
    #{req_start := StartTime} = CowboyMetrics,

    %% TODO: get the informational data from the list of informational data.
    LD2 = LD1#log_data{
        reason = switch_protocol,
        status = 101,
        response_body_length = 0,
        start_time = StartTime + erlang:time_offset()
    },
    z_buffered_worker:push(?MODULE, LD2);

log_access(#{reason := {ErrorReason, _, _}} = CowboyMetrics) ->
    #{resp_status := Status, resp_body_length := RespBodyLength} = CowboyMetrics,

    LD1 = metrics_to_log_data(CowboyMetrics),
    LD2 = LD1#log_data{
        reason = ErrorReason,
        status = Status,
        start_time = start_time(CowboyMetrics) + erlang:time_offset(),
        response_body_length = RespBodyLength
    },
    z_buffered_worker:push(?MODULE, LD2);

log_access(#{reason := Reason} = CowboyMetrics) ->
    LD1 = #{resp_status := Status, resp_body_length := RespBodyLength} = CowboyMetrics,
    LD2 = LD1#log_data{
        reason = Reason,
        status = Status,
        start_time = start_time(CowboyMetrics) + erlang:time_offset(),

        response_body_length = RespBodyLength
    },
    z_buffered_worker:push(?MODULE, LD2).

%%
%% Buffered worker callbacks
%%

% @doc Initialize the logger.
init(_Pid, [[Name, Opts, Facility], Priority]) ->
    {ok, Log} = syslog:open(Name, Opts, Facility),
    {ok, ?FLUSH_INTERVAL, #state{priority=Priority, log=Log}}.

% @doc Log one entry
handle_value(_Pid, _Count, LogData, #state{log=Log, priority=Priority, native_to_second=NTS}) ->
    Msg = format(LogData, NTS),
    syslog:log(Log, Priority, Msg).

% @doc Flush operation done.
handle_flush_done(_Pid, _State) ->
    ok.

%%
%% Helpers
%%

% @doc Return the start time of the request
start_time(#{early_error_time := Time}) -> Time;
start_time(#{req_start := Time}) -> Time.

metrics_to_log_data(#{req := Req}) -> request_to_log_data(Req);
metrics_to_log_data(#{partial_req := PartialReq}) ->
    {Host, UserAgent, Referer} = case maps:get(headers, PartialReq, undefined) of
        undefined -> {undefined, undefined, undefined};
        Headers ->
            H = maps:get(<<"host">>, Headers, undefined),
            U = maps:get(<<"user-agent">>, Headers, undefined),
            R = maps:get(<<"referer">>, Headers, undefined),
            {H, U, R}
    end,

    #log_data{
        peer = maps:get(peer, PartialReq, undefined),
        version = maps:get(version, PartialReq, undefined),
        method = maps:get(method, PartialReq, undefined),
        scheme = maps:get(scheme, PartialReq, undefined),
        qs = maps:get(qs, PartialReq, undefined),

        host = Host,
        referer = Referer,
        user_agent = UserAgent
    }.

% @doc Get information from the request into a log_data record.
request_to_log_data(Req) ->
    #{peer := Peer, method := Method, version := Version, scheme := Scheme, path := Path, qs := Qs} = Req,
    #{headers := Headers} = Req,

    Host = maps:get(<<"host">>, Headers, undefined),
    UserAgent = maps:get(<<"user-agent">>, Headers, undefined),
    Referer = maps:get(<<"referer">>, Headers, undefined),

    #log_data{
        peer = Peer,
        version = Version,
        host = Host,
        scheme = Scheme,
        method = Method,
        path = Path,
        qs = Qs,
        user_agent = UserAgent,
        referer = Referer
    }.

% @doc Format the the log message.
format(#log_data{
        reason = Reason,
        peer = {RemoteIP, RemotePort},
        scheme = Scheme,
        host = Host,
        status = Status,
        method = Method,
        path = Path,
        qs = Qs,
        version = Version,

        request_body_length = ReqSize,
        response_body_length = Size,

        referer = Referer,
        user_agent = UserAgent,

        start_time = StartTime}, NTS) ->

    Timestamp = calendar:gregorian_seconds_to_datetime((StartTime div NTS) + ?EPOCH),

    RawPath = case Qs of
        undefined -> Path;
        <<>> -> Path;
        _ -> <<Path/binary, $?, Qs/binary>>
    end,

    Site = case get_site(Host) of
        {ok, S} -> z_convert:to_binary(S);
        undefined -> undefined
    end,

    format(
        fmt_time(Timestamp),
        RemoteIP, RemotePort,
        option_fmt(Scheme),
        option_fmt(Host),
        option_fmt(Site),
        option_fmt(Method),
        option_fmt(RawPath),
        option_fmt(Version),
        fmt_reason(Reason),
        option_fmt(Status),
        option_fmt(ReqSize),
        option_fmt(Size),
        option_fmt(Referer),
        option_fmt(UserAgent)).

format(Time, Ip, Port, Scheme, Host, Site, Method, Path, Version, Reason, Status, ReqSize, Length, Referrer, UserAgent) ->
    [fmt_ip(Ip), $:, z_convert:to_binary(Port), $\s,
    Time, $\s,
    Scheme, $\s,
    Host, $:, Site, $\s,
    $", sanitize(Method), " ", sanitize(Path), $\s, Version, $", $\s,
    Reason, $\s,
    Status, $\s,
    ReqSize, $/, Length, $\s,
    $", sanitize(Referrer), $",$\s,
    $", sanitize(UserAgent), $"].

% @doc format optional values. When the value is undefined output a "-"
option_fmt(undefined) -> <<"-">>;
option_fmt(Bin) when is_binary(Bin) -> Bin;
option_fmt(D) -> z_convert:to_binary(D).

% @doc Format the request reason.
fmt_reason(normal) -> $N;
fmt_reason(switch_protocol) -> $P;
fmt_reason(internal_error) -> <<"EI">>;
fmt_reason(socket_error) -> <<"ES">>;
fmt_reason(stream_error) -> <<"ER">>;
fmt_reason(connection_error) -> <<"EC">>;
fmt_reason(stop) -> $S;
fmt_reason(_) -> $*.

% @doc Format an ip address
fmt_ip(IP) when is_tuple(IP) -> inet_parse:ntoa(IP);
fmt_ip(undefined) -> <<"0.0.0.0">>;
fmt_ip(HostName) -> z_convert:to_binary(HostName).

% @doc Format a timestamp
fmt_time(undefined) -> <<"-">>;
fmt_time({{Year, Month, Date}, {Hour, Min, Sec}}) ->
    io_lib:format("[~2..0w/~s/~4..0w:~2..0w:~2..0w:~2..0w ~s]",
        [Date, month(Month), Year, Hour, Min, Sec, "+0000"]).

% @doc Prevent escape characters to be shown in the log file.
% Seealso http://cve.mitre.org/cgi-bin/cvename.cgi?name=CVE-2009-4487
sanitize(S) -> sanitize(S, <<>>).

sanitize(undefined, Acc) -> Acc;
sanitize(<<>>, Acc) -> Acc;
sanitize(<<C, Rest/binary>>, Acc) when C < 32 ->
    sanitize(Rest, <<Acc/binary, (C+$A), $^>>);
sanitize(<<C, Rest/binary>>, Acc) ->
    sanitize(Rest, <<Acc/binary, C>>).

month(1) -> <<"Jan">>;
month(2) -> <<"Feb">>;
month(3) -> <<"Mar">>;
month(4) -> <<"Apr">>;
month(5) -> <<"May">>;
month(6) -> <<"Jun">>;
month(7) -> <<"Jul">>;
month(8) -> <<"Aug">>;
month(9) -> <<"Sep">>;
month(10) -> <<"Oct">>;
month(11) -> <<"Nov">>;
month(12) -> <<"Dec">>.

get_site(Host) ->
    case z_sites_dispatcher:get_site_for_hostname(Host) of
        {ok, _}=S -> S;
        undefined ->
            case z_sites_dispatcher:get_fallback_site() of
                {ok, _}=S -> S;
                undefined -> undefined
            end
    end.
