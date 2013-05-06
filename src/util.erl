-module(util).

-export([is_process_alive/1, shuffle_lists/1, lists_set_nth/3, rearrange_lists/2]).

-export([log_incoming_msg/6, time_diff/1]).

is_process_alive(Pid) when is_pid(Pid) ->
	rpc:call(node(Pid),erlang,is_process_alive,[Pid]);
is_process_alive(_) ->
	false.

time_diff(Time1) ->
    {_, S1, MS1} = Time1,
    {_, S2, MS2} = erlang:now(),
    (S2-S1)*1000000+(MS2 - MS1).

shuffle_lists(List) ->
	List1 = [{random:uniform(), X} || X <- List],
	List2 = lists:keysort(1, List1),
	[E || {_, E} <- List2].

lists_set_nth(N, Element, List) ->
	Fun = fun(E, Acc) -> case Acc of N -> {Element, (Acc+1)}; _ -> {E, (Acc+1)} end end,
	{List2, _} = lists:mapfoldl(Fun, 1, List),
	List2.

rearrange_lists(N, List) ->
	{Part1, Part2} = lists:split(N, List),
	lists:append(Part2, Part1).

log_incoming_msg(_Module, _ModuleFrom, _From, timer, _State, _Method) ->
	ok;
log_incoming_msg(_Module, _ModuleFrom, _From, check_tcp_alive, _State, _Method) ->
	ok;
log_incoming_msg(_Module, _ModuleFrom, _From, check_player_alive, _State, _Method) ->
	ok;
log_incoming_msg(Module, ModuleFrom, From, Msg, State, Method) ->
	dm_log:debug("=========== ~p process_protocol ===========~nModule: ~p~n"++
		"From: ~p~nMsg: ~p~nState: ~p~nMethod: ~p~n======================================== ~n", 
		[Module, ModuleFrom, From, Msg, State, Method]).