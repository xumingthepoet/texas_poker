-module(game).

-export([start_game/0, end_game/0, on_message/1, on_timer/0]).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{start_game, 0}, {end_game, 0}, {on_message, 1}, {on_timer, 0}];
behaviour_info(_) ->
    undefined.

start_game() ->
	ok.

end_game() ->
	ok.

on_message(_Msg) ->
	ok.

on_timer() ->
	ok.