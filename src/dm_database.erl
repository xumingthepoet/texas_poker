-module(dm_database).

-include("dm_database.hrl").

-export([init/0, get_player_info/1, set/1, add_player_money/2]).

init() ->
	database:init().

get_player_info(UID) ->
	try
		Info = database:get(UID),
		#player_info{uid=UID, money=Info}
	catch
		_:_ ->
			undefined
	end.

set(Key) ->
	ok.

add_player_money(UID, Score) ->
	ok.