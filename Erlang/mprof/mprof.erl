%% Copyright 2010 Martin Carlson. All rights reserved.
%% 
%% Redistribution and use in source and binary forms, with or without modification, are
%% permitted provided that the following conditions are met:
%% 
%%    1. Redistributions of source code must retain the above copyright notice, this list of
%%       conditions and the following disclaimer.
%% 
%%    2. Redistributions in binary form must reproduce the above copyright notice, this list
%%       of conditions and the following disclaimer in the documentation and/or other materials
%%       provided with the distribution.
%% 
%% THIS SOFTWARE IS PROVIDED BY MARTIN CARLSON AS IS'' AND ANY EXPRESS OR IMPLIED
%% WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
%% FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL MARTIN CARLSON OR
%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
%% ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%% 
%% The views and conclusions contained in the software and documentation are those of the
%% authors and should not be interpreted as representing official policies, either expressed
%% or implied, of Martin Carlson.
-module(mprof).
-export([delta_stats/1, delta_stats/2, delta_stats_report/1]).
-export([delta_tracer/2, delta/5, delta_stop/0]).
-export([trace/3, stop/1]).

%% ERTS return_trace match spec.
-define(RETURN_PATTERN, {'_',[],[{return_trace}]}).


%% -----------------------------------------------------------------
%% Purpose: Trace the time deltaerence between an
%%          accumulator function and an over all function.
%% Example: mprof:delta(wheris(server), server, connect, gen_tcp, connect).
%% -----------------------------------------------------------------
delta(Proc, OM, OF, AM, AF) ->
	trace(delta_tracer, Proc, delta_tracer({OM, OF, '_'}, {AM, AF, '_'})).


%% -----------------------------------------------------------------
%% Purpose: Stop the running delta_tracer.
%% -----------------------------------------------------------------
delta_stop() ->
	stop(delta_tracer).


%% -----------------------------------------------------------------
%% Purpose: Tracer for tracing the time deltaerence between an
%%          accumulator function and an over all function.
%% -----------------------------------------------------------------
delta_tracer({OM, OF, _} = Overall, Acc) ->
	fun(init) ->
		    {[Overall, Acc], delta_stats(init)};
	   ({terminate, _}) ->
		    ok;
	   ({{call, _, M, F, _}, _}) when M == OM, F == OF ->
			delta_stats(init);
	   ({{call, _, _, _, _}, St}) ->
			delta_stats(call, St);
	   ({{return, _, M, F, _, _}, St}) when M == OM, F == OF ->
			delta_stats_report(delta_stats(get, St)),
			St;
		({{return, _, _, _, _, _}, St}) ->
			delta_stats(return, St);
		({{info, Msg}, St}) ->
			io:format("Unknown message: ~p~n", [Msg]),
			delta_stats_report(delta_stats(get, St)),
			St
	end.


%% -----------------------------------------------------------------
%% Purpose: Calculate an accumulated call time vs an overall call time.
%% -----------------------------------------------------------------
delta_stats(init) ->
	delta_stats(reset);
delta_stats(reset) ->
	{now(), 0, 0, 0}.
delta_stats(call, {Overall, _, AccT, AccC}) ->
	{Overall, now(), AccT, AccC};
delta_stats(return, {Overall, Last, AccT, AccC}) ->
	{Overall, 0, AccT + timer:now_diff(now(), Last), AccC + 1};
delta_stats(get, {Overall, _, AccT, AccC}) ->
	{timer:now_diff(now(), Overall), AccT, AccC}.


%% -----------------------------------------------------------------
%% Purpose: Print an easy to read report of the overall vs accumulated call time.
%% -----------------------------------------------------------------
delta_stats_report({Overall, AccT, AccC}) ->
	io:format("Overall: ~.4f sec Accumulated: ~.4f sec (~.2f %) Count: ~p~n",
	          [Overall / 1000000, AccT / 1000000,
	           AccT / Overall * 100, AccC]).


%% -----------------------------------------------------------------
%% Purpose: Stop a tracer
%% -----------------------------------------------------------------
stop(Tracer) ->
	case whereis(Tracer) of
		undefined -> ok;
		Pid       -> exit(Pid, shutdown)
	end.


%% -----------------------------------------------------------------
%% Purpose: Run the tracer in a trace process
%% -----------------------------------------------------------------
trace(Name, ProcSet, Tracer) ->
	F = fun() ->
		register(Name, self()),
		process_flag(trap_exit, true),
		{Patterns, St} = Tracer(init),
		try
			trace_on(ProcSet, Patterns),
			Tracer({terminate, trace_loop(Tracer, St)})
		after
			trace_off(ProcSet, Patterns)
		end
	end,
	spawn(F).


%% -----------------------------------------------------------------
%% Purpose: Runs tracer in a loop
%% -----------------------------------------------------------------
trace_loop(Tracer, St) ->
	receive
		{'EXIT', _, _} ->
			St;
		{trace, CP, call, {CM, CF, CA}} ->
			trace_loop(Tracer, Tracer({{call, CP, CM, CF, CA}, St}));
		{trace, CP, return_from, {CM, CF, CA}, CR} ->
			trace_loop(Tracer, Tracer({{return, CP, CM, CF, CA, CR}, St}));
		Msg ->
			trace_loop(Tracer, Tracer({{info, Msg}, St}))
	end.


%% -----------------------------------------------------------------
%% Purpose: Turn trace flags and patterns on
%% -----------------------------------------------------------------
trace_on(ProcSet, Patterns) ->
	erlang:trace(ProcSet, true, [call]),
	F = fun(Pattern) ->
		       erlang:trace_pattern(Pattern, [?RETURN_PATTERN], [global])
	    end,
	lists:foreach(F, Patterns).


%% -----------------------------------------------------------------
%% Purpose: Turn trace flags and patterns off
%% -----------------------------------------------------------------
trace_off(ProcSet, Patterns) ->
	erlang:trace(ProcSet, false, [call]),
	F = fun(Pattern) ->
		       erlang:trace_pattern(Pattern, false, [global])
	    end,
	lists:foreach(F, Patterns).
