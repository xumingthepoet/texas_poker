-module(dm_game).

%%===================================================
%% A middle man between generic code and custom code
%%===================================================

-include("custom_protocol.hrl").
-include("dm_protocol.hrl").
-include("dm_database.hrl").

-export([on_player_message/5]).

on_player_message(Mod, Tcp, Play_info, Game_info, Message) ->
	#game_info{room_id = Room_pid, info = Game_info2} = Game_info,
	{P_info2, Game_info3} = Mod:on_player_message(Tcp, Play_info, Room_pid, Game_info2, Message),
	case Game_info3 of
		undefined -> 
			{Play_info = P_info2, undefined};
		_ ->
			{Play_info = P_info2, Game_info#game_info{info = Game_info3}}
	end.
    
