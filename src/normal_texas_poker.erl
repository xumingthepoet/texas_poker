-module(normal_texas_poker).

-include("custom_protocol.hrl").
-include("dm_database.hrl").

-behaviour(gen_game).

-export([on_player_message/5, init_room/0, on_room_message/3, 
	on_room_timer/1, info2client/2, enter_room_request/2]).

%% game_info for Player process, position <- lists:seq(0,9). if position == 0 , the player is standup.
-record(game_info, {position, public_info}).

%% room_info for Room process, cards is ?POKER_CARDS , peolple is Player process that watching , counter is internal variable for timer .
-record(room_info, {cards, people, counter, min_bet, public_info}).

%% public_info will sent every people in the room, state <- [ready, pre_flop, flop, true, river, over ] , active_ or dealer_position <- lists:seq(1,9).
-record(public_info, {state, board, bets, active_position, dealer_position, gamblers}).

%% gamebler is real player model in game , state <- [wating_next_game, waiting_room, wating_client ...], 
%% title <- [name, small_blind, big_blind, call, raise, check, fold, all_in ]
-record(gambler, {pid, state, title, bet, buyin, hand}). 

-define(MAX_WAIT_TIME, 15).
-define(TOTAL_NUMBER_PLYAER, 9).

%%================
%% Player process 
%%================

% message for enter_room_success and reconnection
info2client(Player_info, Game_info) ->
	<<"ok">>.

on_player_message(Tcp, Player_info, Room_pid, Game_info, Msg) ->
	{M} = Msg,
	Api = proplists:get_value(?API2, M),
	Content = proplists:get_value(?CONTENT2, M),
	Extra = proplists:get_value(?EXTRA, M),
	Extra2 = proplists:get_value(?EXTRA2, M),
	case {Api, {Content, Extra, Extra2}, Game_info} of
		{?SIT_DOWN, {Position, Buyin, _}, _} ->
			on_sit_down(Player_info, Game_info, Room_pid, Position, Buyin);
		{sit_down_success, {{Public_info, Position, Buyin, Who}, _, _}, _} ->
			on_sit_down_success(Public_info, Position, Buyin, Who, Tcp, Player_info, Game_info);
		{game_begin, {Public_info, _, _}, _} ->
			on_game_begin(Public_info, Tcp, Player_info, Game_info);
		{blind, {Blind_bet, _, _}, _} ->
			on_blind(Player_info, Game_info, Blind_bet);
		{?STAND_UP, _, _} ->
			{Player_info, Game_info};
		{?LEAVE_ROOM, _, _} ->
			{Player_info, Game_info};
		{?RAISE, _, _} ->
			{Player_info, Game_info};
		{?FOLD, _, _} ->
			{Player_info, Game_info};
		{?CHECK, _, _} ->
			{Player_info, Game_info};
		{?ALL_IN, _, _} ->
			{Player_info, Game_info};
		_ ->
			{Player_info, Game_info}
	end.

%%======================
%% Player Internal API         
%%======================

on_sit_down(Player_info, Game_info ,Room_pid ,Position , Buyin) ->
	Position_old = Game_info#game_info.position,
	case Position_old of
		0 ->	
			dm_player:to_room(Room_pid, {sit_down, Position, Buyin});
		_ ->
			ok
	end,
	{Player_info, Game_info}.

on_sit_down_success(Public_info, Position, Buyin, Who, Tcp, Player_info, Game_info) ->
	case self() of
		Who ->
			dm_player:to_client(Tcp, ?MESSAGE_I_SIT_DOWN_SUCCESS(
				info2client(Player_info, Game_info), Position)),
			{Player_info, Game_info#game_info{position = Position, public_info = Public_info}};
		_ ->
			dm_player:to_client(Tcp, ?MESSAGE_SOMEONE_SIT_DOWN_SUCCESS(
				info2client(Player_info, Game_info), Position)),
			{Player_info, Game_info#game_info{public_info = Public_info}}
	end.

on_game_begin(Public_info, Tcp, Player_info, Game_info) ->
	Game_info2 = Game_info#game_info{public_info = Public_info},
	dm_player:to_client(Tcp, ?MESSAGE_GAME_BEGIN(info2client(Player_info, Game_info2))),
	{Player_info, Game_info2}.

on_blind(Player_info, Game_info, Blind_bet) ->
	Money = Player_info#player_info.money,
	dm_database:add_player_money(Player_info#player_info.uid, - Money),
	{Player_info#player_info{money = Money-Blind_bet}, Game_info}.

%%==============
%% Room process 
%%==============

init_room() ->
	<<A1:32,A2:32,A3:32>> = crypto:rand_bytes(12),
    random:seed(A1,A2,A3),
	#room_info{cards = ?POKER_CARDS, people = [], counter = 0, min_bet = 10, public_info = #public_info{state = ready, board = [],
		 bets = [], dealer_position = 1, active_position = 1, gamblers = [[], [], [], [], [], [], [], [], []]}}.

enter_room_request(Room_info, Player) ->
	People = [Player | lists:delete(Player, Room_info#room_info.people)],
	Room_info1 = Room_info#room_info{people = People},
	Game_info1 = #game_info{position = 0, public_info = Room_info1#room_info.public_info},
	{Room_info1, Game_info1}.
	
on_room_message(Room_info, Msg, From) ->
	case Msg of
		{sit_down, Position, Buyin} ->
			on_sit_down(Room_info, Position, Buyin, From);
		_ ->
			Room_info
	end.

on_room_timer(Room_info) ->
	{Cards, People, Counter, Min_bet, Public_info, State, Gamblers, Dealer_position, 
		Active_position, Bets, Board} = untangle_room_info(Room_info),
	case State of
		ready ->
			on_timer_ready(Room_info, Counter, Public_info, Gamblers, Dealer_position, Min_bet, Bets, People);
		pre_flop ->
			on_timer_pre_flop();
		_ ->
			Room_info
	end.

%%======================
%% Room Internal API         
%%======================
	
on_sit_down(Room_info, Position, Buyin, From) ->
	People = Room_info#room_info.people,
	Public_info = Room_info#room_info.public_info,
	Gamblers = Public_info#public_info.gamblers,
	Gambler = lists:nth(Position, Gamblers),
	case Gambler of
		[] ->
			Gamblers2 = util:lists_set_nth(Position, #gambler{pid=From, state=waiting_next_game, bet=0, buyin = Buyin, hand=[]}, Gamblers),
			Public_info2 = Public_info#public_info{gamblers = Gamblers2},
			dm_room:broadcast(People, player_msg_install(sit_down_success, {Public_info2, Position, Buyin, From})),
			Room_info#room_info{public_info = Public_info2};
		_ ->
			Room_info
	end.

on_timer_ready(Room_info, Counter, Public_info, Gamblers, Dealer_position, Min_bet, Bets, People) ->
	Enough_to_begin = lists:flatlength(Gamblers) > 1,
	case Enough_to_begin of 
		false -> Room_info;
		true -> 
			{Gamblers2, Bets_blind} = set_blind(Gamblers, Dealer_position, Min_bet),
			Public_info2 = Public_info#public_info{gamblers=Gamblers2, bets=[Bets_blind|Bets], state = pre_flop},
			dm_room:broadcast(People, player_msg_install(game_begin, Public_info2)),
			Room_info#room_info{public_info = Public_info2}
	end.

set_blind(Gamblers, Dealer_position, Min_bet) ->
	set_blind_(Gamblers, 0, Dealer_position, 2, ?TOTAL_NUMBER_PLYAER, Min_bet). %% 2 for small-blind and 1 for big-blind

set_blind_(Gamblers, Bets_blind, _, _, 0, Min_bet) ->
	{Gamblers, Bets_blind};
set_blind_(Gamblers, Bets_blind, Dealer_position, N, L, Min_bet) ->
	Position = (Dealer_position+1) rem ?TOTAL_NUMBER_PLYAER,
	Gambler = lists:nth(Position, Gamblers),
	case Gambler of 
		[] -> set_blind_(Gamblers, Bets_blind, Position, N, L-1, Min_bet);
		_ ->
			Pid = Gambler#gambler.pid,
			Buyin = Gambler#gambler.buyin,
			case {Gambler#gambler.state, N} of
				{waiting_next_game, _} ->
					dm_room:to_player(Pid, player_msg_install(blind, Min_bet)),
					set_blind_(util:lists_set_nth(Position, Gambler#gambler{bet=Min_bet, title=big_blind, state=waiting_room, buyin=Buyin-Min_bet}, Gamblers),
						Bets_blind+Min_bet, Position, N, L-1, Min_bet);
				{_, 2} ->
					dm_room:to_player(Pid, player_msg_install(blind, Min_bet/2)),
					set_blind_(util:lists_set_nth(Position, Gambler#gambler{bet=Min_bet/2, title=small_blind, buyin=Buyin-Min_bet/2}, Gamblers), 
						Bets_blind+Min_bet/2, Position, 1, L-1, Min_bet);
				{_, 1} ->
					dm_room:to_player(Pid, player_msg_install(blind, Min_bet)),
					set_blind_(util:lists_set_nth(Position, Gambler#gambler{bet=Min_bet, title=big_blind, buyin=Buyin-Min_bet}, Gamblers), 
						Bets_blind+Min_bet, Position, 0, L-1, Min_bet);
				_ ->
					set_blind_(Gamblers, Bets_blind, Position, 0, L-1, Min_bet)
			end
	end.

on_timer_pre_flop() ->
	ok.



%%======================
%% Internal API         
%%======================

player_msg_install(Api, Content) ->
	{[{?API2, Api}, {?CONTENT2, Content}]}.

player_msg_install(Api, Content, Extra) ->
	{[{?API2, Api}, {?CONTENT2, Content}, {?EXTRA, Extra}]}.

player_msg_install(Api, Content, Extra, Extra2) ->
	{[{?API2, Api}, {?CONTENT2, Content}, {?EXTRA, Extra}, {?EXTRA2 ,Extra2}]}.

untangle_room_info(Room_info) ->
	{Cards, People, Counter, Min_bet, Public_info} = {Room_info#room_info.cards, Room_info#room_info.people, 
		Room_info#room_info.counter, Room_info#room_info.min_bet, Room_info#room_info.public_info},
	{State, Gamblers, Dealer_position, Active_position, Bets, Board} = untangle_public_info(Public_info),
	{Cards, People, Counter, Public_info, State, Gamblers, Dealer_position, Active_position, Bets, Board}.

untangle_public_info(Public_info) -> 
   {State = Public_info#public_info.state,
	Gamblers = Public_info#public_info.gamblers,
	Dealer_position = Public_info#public_info.dealer_position,
	Active_position = Public_info#public_info.active_position,
	Bets = Public_info#public_info.bets,
	Board = Public_info#public_info.board}.

untangle_gambler_info(Gambler) -> 
	{Pid, State, Title, Bet, Buyin, Hand} = {Gambler#gambler.pid, Gambler#gambler.state, Gambler#gambler.title,
		Gambler#gambler.bet, Gambler#gambler.buyin, Gambler#gambler.hand}.



