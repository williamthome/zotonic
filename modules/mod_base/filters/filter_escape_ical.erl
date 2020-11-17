%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'escape_ical' filter, escape a value for an ical file.

%% Copyright 2010 Marc Worrell
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

-module(filter_escape_ical).
-export([escape_ical/2]).


escape_ical(undefined, _Context) ->
	<<>>;
escape_ical(In, _Context) ->
	escape_ical(In).

escape_ical(L) when is_list(L) ->
    escape_ical(iolist_to_binary(L));
escape_ical(B) when is_binary(B) ->
    escape_ical(B, <<>>, 0);
escape_ical(A) when is_atom(A) ->
    escape_ical(atom_to_list(A)).

escape_ical(<<>>, Acc, _N) -> Acc;
escape_ical(B, Acc, N) when N >= 70 -> escape_ical(B, <<Acc/binary, 13, 10, 32>>, 0);
escape_ical(<<13, 10, Rest/binary>>, Acc, N) -> escape_ical(Rest, <<Acc/binary, $\\, $n>>, N+2);
escape_ical(<<10, Rest/binary>>, Acc, N) -> escape_ical(Rest, <<Acc/binary, $\\, $n>>, N+2);
escape_ical(<<9, Rest/binary>>, Acc, N) -> escape_ical(Rest, <<Acc/binary, 32>>, N+1);
escape_ical(<<$", Rest/binary>>, Acc, N) -> escape_ical(Rest, <<Acc/binary, $\\, $">>, N+2);
escape_ical(<<$,, Rest/binary>>, Acc, N) -> escape_ical(Rest, <<Acc/binary, $\\, $,>>, N+2);
escape_ical(<<$:, Rest/binary>>, Acc, N) -> escape_ical(Rest, <<Acc/binary, $\\, $:>>, N+3);
escape_ical(<<$;, Rest/binary>>, Acc, N) -> escape_ical(Rest, <<Acc/binary, $\\, $;>>, N+2);
escape_ical(<<$\\, Rest/binary>>, Acc, N) -> escape_ical(Rest, <<Acc/binary, $\\, $\\>>, N+2);
escape_ical(<<C, Rest/binary>>, Acc, N) -> escape_ical(Rest, <<Acc/binary, C>>, N+1).
