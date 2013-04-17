-module(dm_log).

-export ([debug/1, debug/2, console/1, console/2, backend/1, backend/2, error/1, error/2]).

debug(Msg) ->
	dm_log:error(Msg).	

debug(Text, Args) ->
	dm_log:error(Text, Args).

console(Msg) ->
	dm_log:error(Msg).	

console(Text, Args) ->
	dm_log:error(Text, Args).

backend(Msg) ->
	dm_log:error(Msg).

backend(Text, Args) ->
	dm_log:error(Text , Args).

error(Msg) ->
	io:format(Msg).

error(Text, Args) ->
	io:format(Text, Args).
