-module(gen_game).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{on_player_message, 5},
     {init_room, 0},
     {on_room_message, 3}, 
     {on_room_timer, 1},
     {info2client, 2},
     {enter_room_request, 2}];  	

behaviour_info(_) ->
    undefined.


