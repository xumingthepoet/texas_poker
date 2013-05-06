-module(normal_texas_poker).

-include("custom_protocol.hrl").
-include("dm_database.hrl").

-behaviour(gen_game).

-export([on_player_message/5, init_room/0, on_room_message/3, 
	on_room_timer/1, info2client/2, enter_room_request/2, leave_room/3]).

%% game_info for Player process, position <- lists:seq(0,9). if position == 0 , the player is standup.
-record(game_info, {position, public_info}).

%% room_info for Room process, cards is ?POKER_CARDS , peolple is Player process that watching , counter is internal variable for timer .
-record(room_info, {cards, people, counter, one_v_one, public_info}).

%% public_info will sent every people in the room, state <- [ready, pre_flop, flop, true, river, showdown ] , active_ or dealer_position <- lists:seq(1,9).
-record(public_info, {state, blind_bet, board, pot, high_bet, active_position, dealer_position, round_over_position, bets, gamblers}).

%% gamebler is real player model in game , state <- [waiting_next_game, waiting_room, waiting_client ...], 
%% title <- [name, small_blind, big_blind, call, raise, check, fold, all_in ], raised <- [true, false]
-record(gambler, {pid, state, raised, title, buyin, hand}).

-define(MAX_WAIT_TIME, 15).
-define(TOTAL_NUMBER_PLYAER, 9).

%%================
%% Player process 
%%================

% message for enter_room_success and reconnection
info2client(Player_info, Game_info) ->
	Uid = Player_info#player_info.uid,
	Money = Player_info#player_info.money,

	Position = Game_info#game_info.position,
	Public_info = Game_info#game_info.public_info,

	{State, Blind_bet, Gamblers, Dealer_position, Active_position, Pot, Board} = untangle_public_info(Public_info),

	Json = {[{position, Position}, {state, State}, {blind_bet, Blind_bet}, {d_p, Dealer_position},
	 {pot, Pot}, {board, cards_to_json(Board)}, {gamblers, gamblers_to_json(Gamblers)}]},

	dm_protocol:msg2json(Json).

leave_room(Room_info, _Player_game_info, _Player) ->
	Room_info.

cards_to_json(Board) ->
	lists:map(fun ({T,N}) -> [T,N] end, Board).

gamblers_to_json(Gamblers) ->
	lists:map(fun (G) -> [G#gambler.state, G#gambler.raised, G#gambler.title, G#gambler.buyin, 
		cards_to_json(G#gambler.hand)] end, Gamblers).

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
			on_stand_up(Player_info, Game_info, Room_pid);
		{someone_stand_up, {Public_info, _, _}, _} ->
			on_someone_stand_up(Public_info, Tcp, Player_info, Game_info);
		{pre_flop, {Public_info, _, _}, _} ->
			on_pre_flop(Public_info, Tcp, Player_info, Game_info);
		{?RAISE, {Times_of_blind_bet, _, _}, _} ->
			on_player_action(Game_info, Room_pid, ?RAISE, Times_of_blind_bet ),
			{Player_info, Game_info};
		{?FOLD, _, _} ->
			on_player_action(Game_info, Room_pid, ?FOLD, undefined),
			{Player_info, Game_info};
		{?CALL, _, _} ->
			on_player_action(Game_info, Room_pid, ?CALL, undefined),
			{Player_info, Game_info};
		{?CHECK, _, _} ->
			on_player_action(Game_info, Room_pid, ?CHECK, undefined),
			{Player_info, Game_info};
		{?ALL_IN, _, _} ->
			on_player_action(Game_info, Room_pid, ?ALL_IN, undefined),
			{Player_info, Game_info};
		{?LEAVE_ROOM, _, _} ->
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
			Game_info2 = Game_info#game_info{position = Position, public_info = Public_info},
			dm_player:to_client(Tcp, ?MESSAGE_I_SIT_DOWN_SUCCESS(
				info2client(Player_info, Game_info), Position)),
			{Player_info, Game_info2};
		_ ->
			Game_info2 = Game_info#game_info{public_info = Public_info},
			dm_player:to_client(Tcp, ?MESSAGE_SOMEONE_SIT_DOWN_SUCCESS(
				info2client(Player_info, Game_info), Position)),
			{Player_info, Game_info2}
	end.

on_game_begin(Public_info, Tcp, Player_info, Game_info) ->
	Game_info2 = Game_info#game_info{public_info = Public_info},
	dm_player:to_client(Tcp, ?MESSAGE_GAME_BEGIN(info2client(Player_info, Game_info2))),
	{Player_info, Game_info2}.

on_blind(Player_info, Game_info, Blind_bet) ->
	Money = Player_info#player_info.money,
	dm_database:add_player_money(Player_info#player_info.uid, - Blind_bet),
	{Player_info#player_info{money = Money-Blind_bet}, Game_info}.

on_stand_up(Player_info, Game_info, Room_pid) ->
	Position_old = Game_info#game_info.position,
	case Position_old of
		0 ->	
			ok;
		_ ->
			dm_player:to_room(Room_pid, {stand_up, Position_old})
	end,
	{Player_info, Game_info#game_info{position=0}}.

on_someone_stand_up(Public_info, Tcp, Player_info, Game_info ) ->
	Game_info2 = Game_info#game_info{public_info = Public_info},
	dm_player:to_client(Tcp, ?MESSAGE_GAME_STATE_CHANGE(info2client(Player_info, Game_info2))),
	{Player_info, Game_info2}.

on_pre_flop(Public_info, Tcp, Player_info, Game_info) ->
	Game_info2 = Game_info#game_info{public_info = Public_info},
	dm_player:to_client(Tcp, ?MESSAGE_GAME_STATE_CHANGE(info2client(Player_info, Game_info2))),
	{Player_info, Game_info2}.

on_player_action(Game_info, Room_pid, Action, Extra) ->
	case Game_info#game_info.position of 
		0 -> ok;
		Position -> dm_player:to_room(Room_pid, {gambler_action, Position, Action, Extra})
	end.

%%==============
%% Room process 
%%==============

init_room() ->
		#room_info{cards = ?POKER_CARDS, people = [], counter = 0,  public_info = #public_info{state = ready, blind_bet = 10, board = [],
		 pot = 0, dealer_position = 1, active_position = 1, bets = [0, 0, 0, 0, 0, 0, 0, 0, 0],
		 gamblers = [[], [], [], [], [], [], [], [], []]}}.

enter_room_request(Room_info, Player) ->
	People = [Player | lists:delete(Player, Room_info#room_info.people)],
	Room_info1 = Room_info#room_info{people = People},
	Game_info1 = #game_info{position = 0, public_info = Room_info1#room_info.public_info},
	{Room_info1, Game_info1}.
	
on_room_message(Room_info, Msg, From) ->
	case Msg of
		{sit_down, Position, Buyin} ->
			on_sit_down(Room_info, Position, Buyin, From);
		{stand_up, Position} ->
			on_gambler_stand_up(Room_info, Position, From);
		{gambler_action, Position, Action, Extra} ->	
			on_gambler_action(Room_info, Position, Action, Extra, From);
		_ ->
			Room_info
	end.

on_room_timer(Room_info) ->
	Public_info = Room_info#room_info.public_info,
	State = Public_info#public_info.state,
	case State of
		ready ->
			on_timer_ready(Room_info);
		pre_flop ->
			on_timer_pre_flop(Room_info);
		flop ->
			on_timer_flop();
		turn ->
			on_timer_turn();
		river ->
			on_timer_river();
		showdown ->
			on_timer_over();
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
	Gamblers_left = lists:flatlength(Gamblers),
	case Gamblers_left of 
		0 -> Dealer_position = Position;
		_ -> Dealer_position = Public_info#public_info.dealer_position
	end,
	case Gambler of
		[] ->
			Gamblers2 = util:lists_set_nth(Position, #gambler{pid=From, state=waiting_next_game, buyin = Buyin, hand=[]}, Gamblers),
			Public_info2 = Public_info#public_info{gamblers = Gamblers2, dealer_position = Dealer_position},
			dm_room:broadcast(People, player_msg_install(sit_down_success, {Public_info2, Position, Buyin, From})),
			Room_info#room_info{public_info = Public_info2};
		_ ->
			Room_info
	end.

on_gambler_stand_up(Room_info, Position, From) -> %% wrong !\
	People = Room_info#room_info.people,
	Public_info = Room_info#room_info.public_info,
	Gamblers = Public_info#public_info.gamblers,
	Gamblers2 = util:lists_set_nth(Position, [], Gamblers),
	Gambler_left = lists:flatlength(Gamblers2), %% wrong way to 
	case Gambler_left of
		0 ->
			Public_info2 = Public_info#public_info{gamblers = Gamblers2, state=ready};
		1 ->
			Public_info2 = goto_over(Public_info#public_info{gamblers = Gamblers2}); 
		_ ->
			Public_info2 = Public_info#public_info{gamblers = Gamblers2}
	end,
	dm_room:broadcast(People, player_msg_install(someone_stand_up, Public_info2)),
	Room_info#room_info{public_info = Public_info2}.

goto_over(Public_info) ->
	Public_info.

on_gambler_action(Room_info, Position, Action, Extra, From) ->
	Public_info = Room_info#room_info.public_info,
	Gamblers = Public_info#public_info.gamblers,
	Gambler = lists:nth(Position, Gamblers),
	case Gambler of 
		[] -> Room_info;
		_ ->
			Pid = Gambler#gambler.pid,
			State = Gambler#gambler.state,
			case {Pid, State} of 
				{From, waiting_client} ->
					on_gambler_action(Room_info, Position, Public_info, Gamblers, Gambler, Action, Extra);
				_ ->
					Room_info
			end
	end.

on_gambler_action(Room_info, Position, Public_info, Gamblers, Gambler, Action, Extra) ->
	case Action of 
		%?RAISE -> 
		?FOLD -> on_gambler_action_fold(Room_info, Position, Public_info, Gamblers, Gambler);
		%?CALL ->
		%?CHECK ->
		%?ALL_IN ->
		_ -> Room_info
	end.

on_gambler_action_fold(Room_info, Position, Public_info, Gamblers, Gambler) ->
	Gambler2 = Gamblers#gambler{title = fold, state = waiting_room},
	Gamblers2 = util:lists_set_nth(Position, Gambler2, Gamblers),
	Active_position2 = 
	Room_info.

next_active(Gamblers, Active_position) ->
	Active_position2 = Active_position rem ?TOTAL_NUMBER_PLYAER + 1,
	Gambler = lists:nth(Active_position2, Gamblers),
	case Gambler of 
		[] -> next_active(Gamblers, Active_position2);
		_ -> 
			Active_position2
	end.

on_timer_ready(Room_info) ->
	{Cards, People, Counter, Public_info, State, Blind_bet, Gamblers, Dealer_position, 
		Active_position, Pot, Board} = untangle_room_info(Room_info),
	Enough_to_begin = lists:flatlength(Gamblers) > 1,
	case Enough_to_begin of 
		false -> Room_info; 
		true -> 
			Dealer_position2 = set_new_dealer(Gamblers, Dealer_position),
			Active_position2 = set_new_active(Gamblers, Dealer_position2),
			{Cards2, Gamblers2} = deal_new_cards(Gamblers),
			{Gamblers3, Bets_blind} = set_blind(Gamblers2, Dealer_position2, Blind_bet),
			Public_info2 = Public_info#public_info{gamblers=Gamblers3, pot=[Bets_blind|Pot], 
			 dealer_position = Dealer_position2, active_position = Active_position2, state = pre_flop},
			dm_room:broadcast(People, player_msg_install(game_begin, Public_info2)),
			Room_info#room_info{public_info = Public_info2, counter=(1+?MAX_WAIT_TIME)}
	end.

set_new_dealer(Gamblers, Dealer_position) ->
	Dealer_position2 = (Dealer_position - 1) rem ?TOTAL_NUMBER_PLYAER + 1,
	Gambler = lists:nth(Dealer_position2, Gamblers),
	case Gambler of 
		[] -> set_new_dealer(Gamblers, Dealer_position2+1);
		_ -> Dealer_position2
	end.

set_new_active(Gamblers, Dealer_position) ->
	set_new_active_(Gamblers, Dealer_position , 3).

set_new_active_(_, Position, 0) ->
	Position;
set_new_active_(Gamblers, Position, N) ->
	Position2 = Position rem ?TOTAL_NUMBER_PLYAER + 1, 
	Gambler = lists:nth(Position2, Gamblers), 
	case Gambler of 
		[] -> set_new_active_(Gamblers, Position2, N);
		_ -> set_new_active_(Gamblers, Position2, N-1)
	end.

deal_new_cards(Gamblers) ->
	Cards = util:shuffle_lists(?POKER_CARDS),
	deal_new_cards(Cards, Gamblers, ?TOTAL_NUMBER_PLYAER).

deal_new_cards(Cards, Gamblers ,0) ->
	{Cards, Gamblers};
deal_new_cards(Cards, Gamblers, N) ->
	Gambler = lists:nth(N, Gamblers),
	case Gambler of 
		[] -> deal_new_cards(Cards, Gamblers, N-1);
		_ ->
			Hand = Gambler#gambler.hand,
			{Two_Cards, Rest_cards} = lists:split(2, Cards),
			Gamblers2 = util:lists_set_nth(N, Gambler#gambler{hand = lists:append(Hand,Two_Cards)}, Gamblers),
			deal_new_cards(Rest_cards, Gamblers2, N-1)
	end.

set_blind(Gamblers, Dealer_position, Blind_bet) ->
	set_blind_(Gamblers, 0, Dealer_position, 2, ?TOTAL_NUMBER_PLYAER, Blind_bet). %% 2 for small-blind and 1 for big-blind

set_blind_(Gamblers, Bets_blind, _, _, 0, Blind_bet) ->
	{Gamblers, Bets_blind};
set_blind_(Gamblers, Bets_blind, Dealer_position, N, L, Blind_bet) ->
	Position = (Dealer_position rem ?TOTAL_NUMBER_PLYAER) + 1,
	Gambler = lists:nth(Position, Gamblers),
	case Gambler of
		[] -> set_blind_(Gamblers, Bets_blind, Position, N, L-1, Blind_bet);
		_ ->
			Pid = Gambler#gambler.pid,
			Buyin = Gambler#gambler.buyin,
			case {Gambler#gambler.state, N} of
				{waiting_next_game, _} ->
					dm_room:to_player(Pid, player_msg_install(blind, Blind_bet)),
					set_blind_(util:lists_set_nth(Position, Gambler#gambler{title=big_blind, state=waiting_room, buyin=Buyin-Blind_bet}, Gamblers),
						Bets_blind+Blind_bet, Position, N, L-1, Blind_bet);
				{_, 2} ->
					dm_room:to_player(Pid, player_msg_install(blind, Blind_bet/2)),
					set_blind_(util:lists_set_nth(Position, Gambler#gambler{title=small_blind, buyin=Buyin-Blind_bet/2}, Gamblers), 
						Bets_blind+Blind_bet/2, Position, 1, L-1, Blind_bet);
				{_, 1} ->
					dm_room:to_player(Pid, player_msg_install(blind, Blind_bet)),
					set_blind_(util:lists_set_nth(Position, Gambler#gambler{title=big_blind, buyin=Buyin-Blind_bet}, Gamblers), 
						Bets_blind+Blind_bet, Position, 0, L-1, Blind_bet);
				_ ->
					set_blind_(Gamblers, Bets_blind, Position, N, L-1, Blind_bet)
			end
	end.

on_timer_pre_flop(Room_info) ->
	{Cards, People, Counter, Public_info, State, Blind_bet, Gamblers, Dealer_position,
	 Active_position, Pot, Board} = untangle_room_info(Room_info),
	case Counter of
		_ when Counter < ?MAX_WAIT_TIME ->
			Room_info#room_info{counter = Counter+1};
		?MAX_WAIT_TIME+1 ->
			Counter1 = 0,
			Active_position2 = get_active_position(Active_position, Gamblers),
			Gambler = lists:nth(Active_position2, Gamblers),
			Gamblers2 = util:lists_set_nth(Active_position2, Gambler#gambler{state=waiting_client}, Gamblers),
			Public_info2 = Public_info#public_info{gamblers = Gamblers2},
			dm_room:broadcast(People, player_msg_install(pre_flop, Public_info2)),
			Room_info#room_info{counter = Counter1, public_info = Public_info2};
		?MAX_WAIT_TIME ->
			Counter1 = 0,
			Active_position2 =  get_active_position(Active_position rem ?TOTAL_NUMBER_PLYAER +1, Gamblers),
			Gambler_old = lists:nth(Active_position, Gamblers),
			case Gambler_old of 
				[] -> Gambler_old2 = [];
				_ -> Gambler_old2 = Gambler_old#gambler{state=waiting_room}
			end,
			Gambler_new = lists:nth(Active_position2, Gamblers),
			Gamblers2 = util:lists_set_nth(Active_position, Gambler_old2, Gamblers),
			Gamblers3 = util:lists_set_nth(Active_position2, Gambler_new#gambler{state=waiting_client}, Gamblers2),
			Public_info2 = Public_info#public_info{gamblers = Gamblers3},
			dm_room:broadcast(People, player_msg_install(pre_flop, Public_info2)),
			Room_info#room_info{counter = Counter1, public_info = Public_info2}
	end.

get_active_position(Active_position, Gamblers) ->
	Gambler = lists:nth(Active_position, Gamblers),
	case Gambler of 
		[] -> get_active_position(Active_position rem ?TOTAL_NUMBER_PLYAER +1, Gamblers);
		_ ->
			Title = Gambler#gambler.title,
			case Title of 
				fold -> get_active_position(Active_position rem ?TOTAL_NUMBER_PLYAER +1, Gamblers);
				_ -> Active_position
			end
	end.


on_timer_flop() ->
	ok.

on_timer_turn() ->
	ok.

on_timer_river() ->
	ok.

on_timer_over() ->
	ok.

go_to_over(Public_info) ->
	Public_info.


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
	{Cards, People, Counter, Public_info} = {Room_info#room_info.cards, Room_info#room_info.people, 
		Room_info#room_info.counter, Room_info#room_info.public_info},
	{State, Blind_bet, Gamblers, Dealer_position, Active_position, Pot, Board} = untangle_public_info(Public_info),
	{Cards, People, Counter, Public_info, State, Blind_bet, Gamblers, Dealer_position, Active_position, Pot, Board}.

untangle_public_info(Public_info) -> 
   {State = Public_info#public_info.state,
    Blind_bet = Public_info#public_info.blind_bet,
	Gamblers = Public_info#public_info.gamblers,
	Dealer_position = Public_info#public_info.dealer_position,
	Active_position = Public_info#public_info.active_position,
	Pot = Public_info#public_info.pot,
	Board = Public_info#public_info.board}.



