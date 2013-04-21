-module(normal_texas_poker).

-include("custom_protocol.hrl").

-behaviour(gen_game).

-export([on_player_message/6, init_room/0, on_room_message/2, 
	on_room_timer/1, info2client/2, enter_room_request/1]).

-record(game_info, {players, cards }).

-record(room_info, {}).

%%================
%% Player process 
%%================

% message for enter_room_success and reconnection
info2client(Player_info, Game_info) ->
	<<"ok">>.

on_player_message(Tcp, UID, Player_info, Room_pid, Game_info, Msg) ->
	case {Game_info, Msg} of
		{_, _} ->
			{Player_info, Game_info};
		_ ->
			{Player_info, Game_info}
	end.

%%==============
%% Room process 
%%==============

enter_room_request(Room_info) ->
	{#room_info{}, #game_info{}}.

init_room() ->
	#room_info{cards = ?POKER}.

on_room_message(Room_info, Msg) ->
	case Msg of
		_ ->
			Room_info
	end. 

on_room_timer(Room_info) ->
	Room_info.

%%======================
%% Internal API 
%%======================





