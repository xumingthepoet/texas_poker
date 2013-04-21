-module(dm_game).

%%======================================
%% A wrapper for gen_game behaviour
%%======================================

-include("custom_protocol.hrl").

-include("dm_protocol.hrl").

-export([on_player_message/5, init_room/1, on_room_message/3, on_room_timer/2, info2client/2]).

on_player_message(Mod, Tcp, Play_info, Game_info, Message) ->
	#player_info{uid = UID, info = P_info} = Play_info,
	#game_info{room_id = Room_pid, info = Game_info2} = Game_info,
	{P_info2, Game_info3} = Mod:on_player_message(Tcp, UID, P_info, Room_pid, Game_info2, Message),
	case Game_info3 of
		undefined -> 
			{Play_info#player_info{info = P_info2}, undefined};
		_ ->
			{Play_info#player_info{info = P_info2}, Game_info#game_info{info = Game_info3}}
	end.

init_room(Mod) ->
	Mod:init_room().

on_room_message(Mod, Game_info, Message) ->
	Mod:on_room_message(Game_info, Message).
	
on_room_timer(Mod, Game_info) ->
	Mod:on_room_timer(Game_info).
    
info2client(Player_info, Game_info) ->
	Mod = Game_info#game_info.room_mod,
	Player_info2 = Player_info#player_info.info,
	Game_info2 = Game_info#game_info.info,
	Mod:info2client(Player_info2, Game_info2).
