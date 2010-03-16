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
-module(mtop).
-export([start/1]).
-import(proplists, [get_value/3]).




%% ----------------------------------------------------------------------------- 
%% Process
%% ----------------------------------------------------------------------------- 


%% Starts the mtop monitoring
%% Example start([{freq, 3000}, {lines, 5}, {sort_on, reductions}]).
start(Options) ->
	Node = get_value(node, Options, node()),
	ensure_loaded(Node),
	spawn(fun() ->
		        register(mtop, self()),
		        Frontend = self(),
		        F = fun() ->
			               backend_loop(get_value(freq, Options, 1000),
			                            Frontend)
		        end,
		        Backend = spawn_link(Node, F),
		        loop(Backend, get_value(lines, Options, 5),
		             get_value(sorter, Options,
		                       sorter(get_value(sort_on, Options,
		                                        [growth, reductions]))))
	      end).


%% ----------------------------------------------------------------------------- 
%% Process
%% ----------------------------------------------------------------------------- 


%% Loops with at most 'Freq' freequenzy,
%% prints the 'Lines' top processes using 'Sorter.
loop(Backend, Lines, Sorter) ->
	receive
		stop ->
			ok;
		{backend, Stats} ->
			print_stats(lists:sublist(lists:sort(Sorter, Stats), 1, Lines)),
			loop(Backend, Lines, Sorter);
		_    ->
			loop(Backend, Lines, Sorter)
	after
		0    -> loop(Backend, Lines, Sorter)
	end.


%% Collects stats from the VM and send a report to the frontend
backend_loop(Freq, Frontend) ->
	Frontend ! {backend, collect(Freq)},
	backend_loop(Freq, Frontend). 


%% ----------------------------------------------------------------------------- 
%% Utility functions
%% ----------------------------------------------------------------------------- 


%% Collects stats over 'Freq' time and collect delta.
collect(Freq) ->
	A = [{P, erlang:process_info(P)} || P <- processes(), P /= self()],
	timer:sleep(Freq),
	[[{proc, P} | growth(Old, erlang:process_info(P))]
	 || {P, Old} <- A, erlang:is_process_alive(P)].


%% Loads the mtop code on 'Node'
ensure_loaded(Node) ->
	if
		Node /= node() ->
			{Mod, Code, File} = code:get_object_code(?MODULE),
			rpc:call(Node, code, load_binary, [Mod, File, Code]),
			rpc:call(Node, code, is_loaded, [Mod]);
		true ->
			true
	end.
	

%% Calculate the growth of a process and annotate it to element 2.
-define(DELTA(KEY, A, B), {KEY, get_value(KEY, B, 0) - get_value(KEY, A, 0)}).
growth(A, B) ->
	[{growth,
	 [?DELTA(heap_size, A, B),
	  ?DELTA(message_queue_len, A, B),
	  ?DELTA(reductions, A, B),
	  ?DELTA(stack_size, A, B),
	  ?DELTA(total_heap_size, A, B)]} | B].
-undef(DELTA).


%% Prints a MFA as standard representation
mfa_to_list({M, F, A}) -> lists:concat([M, ":", F, "/", integer_to_list(A)]).


%% Print the stats of the first 'Lines' processes
print_stats(Procs) when is_list(Procs) ->
	io:put_chars([io_lib:format("~.20s~.10s~.10s~.10s~.25s~n",
	                            ["PID", "RED", "MEM", "MSG", "FUNCTION"]),
	              lists:map(fun print_info_summery/1, Procs)]).


%% Prints a summery line of 'Info'
print_info_summery(Info) ->
	[string:left(pid_to_list(value([proc], Info, exit)), 20),
	 string:left(scale(value([reductions], Info, exit)), 10),
	 string:left(scale(value([heap_size], Info, exit) +
	                   value([stack_size], Info, exit)), 10),
	 string:left(scale(value([message_queue_len], Info, exit)), 10),
	 string:left(mfa_to_list(value([current_function], Info, exit)), 25),
	 $\n].


%% Sorter factory, creates sorter of processes (decending) according to 'Key'
sorter(Key) -> fun(A, B) -> value(Key, A, 0) > value(Key, B, 0) end.


%% Scales a number with postfix (K, M)
scale(N) when N >= 1000000000 -> integer_to_list(N div 1000000000) ++ "G";
scale(N) when N >= 1000000    -> integer_to_list(N div 1000000) ++ "M";
scale(N) when N >= 1000       -> integer_to_list(N div 1000) ++ "K";
scale(N)                      -> integer_to_list(N).


%% Returns the value of 'Path' in 'DeepPropList'
value([Key | T], DeepPropList, Default) ->
	case lists:keysearch(Key, 1, DeepPropList) of
		{value, {Key, Value}} when T == [] ->
			Value;
		{value, {Key, Value}}              ->
			value(T, Value, Default);
		false when Default == exit         ->
			throw({missing_key, Key, DeepPropList});
		false                              ->
			Default
	end.

