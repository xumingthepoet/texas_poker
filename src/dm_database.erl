-module(dm_database).

-include("dm_protocol.hrl").

-export([init/0, get_player_info/1, set/1]).

init() ->
	database:init().

get_player_info(UID) ->
	try
		Info = database:get(UID),
		#player_info{uid=UID, info=Info}
	catch
		_:_ ->
			undefined
	end.

set(Key) ->
	ok.