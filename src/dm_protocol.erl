%%==============================================
%%
%%  This Module encapulate json driver
%%
%%==============================================

-module(dm_protocol).

-include("dm_protocol.hrl").

-export([send/2, encode/1, decode/1, json_new/0, json_new/1, json_get/2, json_set/3]).

%% client will recieve messages ended with "\n" which splits easily.
send(Client, Msg) ->
	try 
		dm_log:backend("Client : ~p, Msg : ~p . ~n", [Client, Msg]),
		gen_tcp:send(Client, iolist_to_binary([Msg, <<"\n">>]))
	catch
		Cls:Err ->
			dm_log:error("tcp send failed. {~p, ~p}~n", [Cls, Err])
	end.

encode(Erlang) ->
	try
		jiffy:encode(Erlang)
	catch
		_:_ ->
			bad_json
	end.

decode(Binary) -> jiffy:decode(Binary).

json_new() ->
	{[]}.

json_new(Proplists) ->
	{Proplists}.

json_get(Key, Json) ->
	{J} = Json,
	proplists:get_value(Key, J).

json_set(Key, Element, Json) ->
	{J} = Json,
	J2 = proplists:delete(Key, J),
	{[{Key, Element} | J2]}.






