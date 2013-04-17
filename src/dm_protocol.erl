%%==============================================
%%
%%  This Module encapulate json driver
%%
%%==============================================

-module(dm_protocol).

-include("dm_protocol.hrl").

-export([send/2, msg2json/1, json2msg/1]).

%% client will recieve messages ended with "\n" which splits easily.
send(Client, Msg) ->
	try 
		gen_tcp:send(Client, iolist_to_binary([Msg, <<"\n">>]))
	catch
		Cls:Err ->
			dm_log:error("tcp send failed. {~p, ~p}", [Cls, Err])
	end.

msg2json(Msg) ->
	try
		jiffy:encode(Msg)
	catch
		_:_ ->
			bad_json
	end.

json2msg(Json) -> jiffy:decode(Json).


