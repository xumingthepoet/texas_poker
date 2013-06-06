-module(normal_texas_poker2).

-include("custom_protocol.hrl").
-include("dm_database.hrl").

-behaviour(gen_game).

-export([on_player_message/5, init_room/0, on_room_message/3, info2client/2, enter_room_request/2, leave_room/3]).
-export([test/0]).

%% game_info for Player process, position <- lists:seq(0,9). if position == 0 , the player is standup.
-record(game_info, {position, room_info}).

%% room_info for Room process, cards is ?POKER_CARDS , peolple is Player process that watching , 
%% state <- [ready, pre_flop, flop, true, river, show_down ] , active_ or dealer_position <- lists:seq(1,9).
-record(room_info, {cards, people, counter, timer, active_position, one_v_one, state, blind_bet, board, 
					pot, high_bet, dealer_position, bets, gamblers, win_info}).

%% gamebler is real player model in game , state <- [waiting_next_game, waiting_room, waiting_client ...], 
%% title <- [name_or_last, small_blind, big_blind, call, raise, check, fold, all_in ]
-record(gambler, {pid, uid, state=waiting_next_game, title=name_or_last, buyin, hand=[], hand_info=[]}).

-define(TOTAL_NUMBER_PLYAER, 9).
-define(WAITING_TIME, 25).
-define(SHOWDOWN_WAITING_TIME, 5).

init_room() ->
	#room_info{cards = [], people = [], counter = 0, timer = undefined, one_v_one = false,  state = ready, blind_bet = 10,
	 board = [], pot = [0, 0, 0, 0, 0, 0, 0, 0, 0], high_bet = 0, dealer_position = 1, active_position = 1,
	 bets = [0, 0, 0, 0, 0, 0, 0, 0, 0], gamblers = [[], [], [], [], [], [], [], [], []], win_info = []}.

enter_room_request(Room_info, Player) ->
	People     = [Player | lists:delete(Player, Room_info#room_info.people)],
	Room_info1 = Room_info#room_info{people = People},
	Game_info  = #game_info{position = 0, room_info = Room_info1},
	{Room_info1, Game_info}.

leave_room(Room_info, Player_game_info, Player) ->  %% TODO: kill the process when nobody exist
	People     = lists:delete(Player, Room_info#room_info.people),
	Room_info1 = Room_info#room_info{people = People},
	case Player_game_info of
		undefined -> Room_info1;
		_ ->
			case Player_game_info#game_info.position of
				0 -> Room_info1;
				Position ->
					Room_info2 = room_stand_up(Position, Player, Room_info1),
					%dm_room:to_player(Player, player_msg_install(room_state_changed, Room_info2)), useless msg since he don't need it anymore
					Room_info2
			end
	end.

% message for enter_room_success and reconnection
info2client(Player_info, Game_info) ->
	Uid       = Player_info#player_info.uid,
	Money     = Player_info#player_info.money,
	
	Position  = Game_info#game_info.position,
	Room_info = Game_info#game_info.room_info,
	
	Counter   = Room_info#room_info.counter,
	One_v_one = Room_info#room_info.one_v_one,
	State     = Room_info#room_info.state,
	Blind_bet = Room_info#room_info.blind_bet,
	Board     = Room_info#room_info.board,
	Pot       = Room_info#room_info.pot,
	High_bet  = Room_info#room_info.high_bet,
	D_p       = Room_info#room_info.dealer_position,
	Bets      = Room_info#room_info.bets,
	Gamblers  = protect_privacy_cards(Position, State , Room_info#room_info.gamblers),
	Win_info  = Room_info#room_info.win_info,

	Json 	  = dm_protocol:json_new([{uid,Uid},{money,Money},{position,Position},{counter,Counter},{ovo,One_v_one},{state,State},{blind,Blind_bet},{board,Board},{pot,lists:sum(Pot)},
				 {high_bet,High_bet},{dealer_position,D_p},{bets,Bets},{gamblers,gamblers_to_json(Gamblers)},{win_info,Win_info}]),
	
	dm_protocol:encode(Json).

protect_privacy_cards(Position, State , Gamblers) ->
	case {Position, State} of
		{_, show_down} ->	
			Fun  = 	fun(G, Acc) -> 
						case Acc of 
							Position -> 
								{G, Acc+1}; 
							_ -> 
								case G of 
									[] -> 
										{[], Acc+1}; 
									_ ->
										Title = G#gambler.title,
										if  Title == fold -> 
												{G#gambler{hand = [], hand_info = []}, Acc+1} ;
											true ->
												{G, Acc+1}
										end
								end 
						end 
				  	end,
			{Gamblers2, _} = lists:mapfoldl(Fun, 1, Gamblers);
		{0, _} -> 
			Gamblers2 = lists:map(fun (G) -> case G of [] -> []; _ -> G#gambler{hand = [], hand_info = []} end end, Gamblers);
		_ -> 
			Fun = fun(G, Acc) -> case Acc of Position -> {G, Acc+1}; _ -> case G of [] -> {[], Acc+1}; _ -> {G#gambler{hand = [], hand_info = []}, Acc+1} end end end,
			{Gamblers2, _} = lists:mapfoldl(Fun, 1, Gamblers) 
	end,
	Gamblers2.

gamblers_to_json(Gamblers) ->
	lists:map(fun (G) -> case G of [] -> 
							dm_protocol:json_new();
							_ -> dm_protocol:json_new([{uid,G#gambler.uid}, {state,G#gambler.state}, {title,G#gambler.title}, {buyin,G#gambler.buyin}, 
								{hand,G#gambler.hand}, {hand_info,G#gambler.hand_info}])
				end end, Gamblers).

on_player_message(Tcp, Player_info, Room_pid, Game_info, Msg) ->
	Api     = dm_protocol:json_get(?API2, Msg),
	Content = dm_protocol:json_get(?CONTENT2, Msg),
	Extra   = dm_protocol:json_get(?EXTRA, Msg),
	Extra2  = dm_protocol:json_get(?EXTRA2, Msg),
	case {Api, {Content, Extra, Extra2}} of
		{?LEAVE_ROOM, _} ->
			dm_player:to_room(Room_pid, {?LEAVE_ROOM, Game_info}),
			{Player_info, undefined};
		{kicked, {_, _, _}} ->
			{Player_info, Game_info#game_info{position=0}};
		{player_state_changed, {change_balance, Diff, _}} ->
			dm_database:add_player_money(Player_info#player_info.uid, Diff),
			NewBalance = Player_info#player_info.money + Diff,
			%dm_player:to_client(Tcp, ?MESSAGE_PLAYER_BALANCE_CHANGED(integer_to_list(NewBalance))),
			{Player_info#player_info{money = NewBalance}, Game_info};
		{room_state_changed, {Room_info, _, _}} ->
			Game_info2 = Game_info#game_info{room_info = Room_info},
			dm_player:to_client(Tcp, ?MESSAGE_GAME_STATE_CHANGE(info2client(Player_info, Game_info2))),
			{Player_info, Game_info2};
		{?SIT_DOWN, {Position, Buyin, _}} when is_integer(Position), is_integer(Buyin) ->
			Old_position = Game_info#game_info.position, 
			Balance 	 = Player_info#player_info.money,
			case {Buyin , Balance, Old_position} of 
				{_, _, 0} when is_integer(Buyin), is_integer(Balance), Buyin =< Balance ->
					dm_player:to_room(Room_pid, {?SIT_DOWN, Position, Player_info#player_info.uid, Buyin});
				_ ->
					ok
			end,
			{Player_info, Game_info};
		{sit_down_success, {Position, _, _}} ->
			{Player_info, Game_info#game_info{position=Position}};
		{?STAND_UP, {_, _, _}} ->
			dm_player:to_room(Room_pid, {?STAND_UP, Game_info#game_info.position}),
			{Player_info, Game_info#game_info{position=0}};
		{?RAISE, {Counter, Bet, _}} when is_integer(Counter), is_integer(Bet) ->
			dm_player:to_room(Room_pid, {?RAISE, Game_info#game_info.position, Counter, Bet}),
			{Player_info, Game_info};
		{?FOLD, {Counter, _, _}} when is_integer(Counter) ->
			dm_player:to_room(Room_pid, {?FOLD, Game_info#game_info.position, Counter}),
			{Player_info, Game_info};
		{?CALL, {Counter, _, _}} when is_integer(Counter) ->
			dm_player:to_room(Room_pid, {?CALL, Game_info#game_info.position, Counter}),
			{Player_info, Game_info};
		{?CHECK, {Counter, _, _}} when is_integer(Counter) ->
			dm_player:to_room(Room_pid, {?CHECK, Game_info#game_info.position, Counter}),
			{Player_info, Game_info};
		{?ALL_IN, {Counter, _, _}} when is_integer(Counter) ->
			dm_player:to_room(Room_pid, {?ALL_IN, Game_info#game_info.position, Counter}),
			{Player_info, Game_info};
		_ ->
			{Player_info, Game_info}
	end.
	
is_counter_and_position_match(Counter, Position, From, Room_info) ->
	is_position_and_state_match(Position, From, Room_info) and is_counter_match(Counter, Room_info).

is_position_and_state_match(Position, From, Room_info)  ->
	Is_position_valid = is_position_valid(Position),
	if Is_position_valid ->
			Gamblers = Room_info#room_info.gamblers,
			Gambler  = lists:nth(Position, Gamblers),
			case Gambler of 
				[] -> fasle;
				_ -> (From == Gambler#gambler.pid) and (waiting_client == Gambler#gambler.state)
			end;
		true -> 
			false
	end.

is_position_valid(Position) ->
	is_integer(Position) and (Position > 0) and (Position =< ?TOTAL_NUMBER_PLYAER).

is_counter_match(Counter, Room_info) ->
	Counter == Room_info#room_info.counter.

on_room_message(Room_info, Msg, From) ->
	case Msg of
		{?LEAVE_ROOM, Game_info} ->
			leave_room(Room_info, Game_info, From);
		{?SIT_DOWN, Position, UID, Buyin}  ->
			case is_position_valid(Position) of 
				true ->	room_sit_down(Position, From, UID, Buyin, Room_info);
				_ ->	Room_info
			end;
		{?STAND_UP, Position}  ->
			case is_position_valid(Position) of
				true ->	room_stand_up(Position, From, Room_info);
				_ -> Room_info
			end;
		{wait_time_out, Counter} ->
			case is_counter_match(Counter, Room_info) of
				true ->	room_wait_time_out(Room_info);
				_ -> Room_info
			end;
		{?RAISE, Position, Counter, Bet}  ->
			case is_counter_and_position_match(Counter, Position, From, Room_info) of
				true ->	room_raise(Position, Bet, Room_info); %%TODO
				_ -> Room_info
			end;
		{?FOLD, Position, Counter} ->
			case is_counter_and_position_match(Counter, Position, From, Room_info) of
				true ->	room_fold(Position, Room_info);
				_ -> Room_info
			end;
		{?CALL, Position, Counter} ->
			case is_counter_and_position_match(Counter, Position, From, Room_info) of
				true ->	room_call(Position, Room_info);
				_ -> Room_info
			end;
		{?CHECK, Position, Counter} ->
			case is_counter_and_position_match(Counter, Position, From, Room_info) of
				true ->	room_check(Position, Room_info);
				_ -> Room_info
			end;
		{?ALL_IN, Position, Counter} ->  
			case is_counter_and_position_match(Counter, Position, From, Room_info) of
				true ->	room_all_in(Position, Room_info); %%TODO
				_ -> Room_info
			end;
		_ ->
			Room_info
	end.

room_wait_time_out(Room_info) ->
	State 	 = Room_info#room_info.state,
	Win_info = Room_info#room_info.win_info,
	case {State, Win_info} of
		{show_down, []} -> 
			Room_info1 = try_begin_game(Room_info),
			dm_room:broadcast(Room_info#room_info.people, player_msg_install(room_state_changed, Room_info1)),
			Room_info1;
		{show_down, _} -> dealing_show_down_time(handle_win_info(Room_info));
		_ -> play_wait_time_out(Room_info)
	end.

handle_win_info(Room_info) ->
	Pots = Room_info#room_info.pot,
	[Pot1|_] = Pots,
	Win_info = Room_info#room_info.win_info,
	[Win_info1| Win_infos2] = Win_info,
	[Pot2, _, Winners] = Win_info1,
	Room_info1 = update_buyin_for_winner(Room_info, Pot2, Winners),
	Room_info2 = Room_info1#room_info{win_info=Win_infos2, pot=util:lists_set_nth(1,Pot1-Pot2,Pots)},
	dm_room:broadcast(Room_info#room_info.people, player_msg_install(room_state_changed, Room_info2)),
	Room_info2.

update_buyin_for_winner(Room_info, Pot, Winners) -> 
	Gamblers  = Room_info#room_info.gamblers,
	Num = lists:flatlength(Winners),
	if 	Num == 0 ->
			Money_split = Pot;
		true ->
			Money_split = Pot div Num
	end,
	Fun  = 	fun (E, Gamblers1) -> 
				Gambler = lists:nth(E, Gamblers1),
				case Gambler of
					[] -> Gambler1 = [];
					_ ->
						Buyin = Gambler#gambler.buyin,
						State = Gambler#gambler.state,
						case State of 
							waiting_next_game ->
								Gambler1 = Gambler;
							_ ->
								dm_room:to_player(Gambler#gambler.pid, player_msg_install(player_state_changed, {change_balance, Money_split})),
								Gambler1 = Gambler#gambler{buyin=Buyin+Money_split}
						end
				end,
				util:lists_set_nth(E, Gambler1, Gamblers1) 
			end,
	Gamblers1 = lists:foldl(Fun, Gamblers, Winners),
	Room_info#room_info{gamblers=Gamblers1}.


dealing_show_down_time(Room_info) ->
	Counter    = Room_info#room_info.counter,
	Room_info2 = Room_info#room_info{counter=Counter+1},
	send_after(Room_info2, ?SHOWDOWN_WAITING_TIME, Room_info2#room_info.counter).

play_wait_time_out(Room_info) ->
	Active_position = Room_info#room_info.active_position,
	Gamblers    = Room_info#room_info.gamblers,
	Gambler     = lists:nth(Active_position, Gamblers),
	High_bet    = Room_info#room_info.high_bet,
	Gambler_bet = lists:nth(Active_position, Room_info#room_info.bets),
	if High_bet > Gambler_bet ->
		    Gambler1 = Gambler#gambler{title = fold, state = waiting_room};
		true ->
		    Gambler1 = Gambler#gambler{title = check, state = waiting_room}
	end,
	Room_info1	= Room_info#room_info{gamblers = util:lists_set_nth(Active_position, Gambler1, Gamblers)},
	Room_info2  = after_action(Room_info1),
	dm_room:broadcast(Room_info2#room_info.people, player_msg_install(room_state_changed, Room_info2)),
	Room_info2.

after_action(Room_info) ->
	Condition = check_room_condition(Room_info),
	case Condition of
		baffler_win ->
			dm_room:broadcast(Room_info#room_info.people, player_msg_install(room_state_changed, Room_info)),
			Room_info1 = set_baffler_win_info(set_hand_info(reset_gamblers_title(fill_pot_and_reset_high_bet(change_game_state(Room_info, show_down))))),
			dealing_show_down_time(Room_info1);
		to_show_down ->
			dm_room:broadcast(Room_info#room_info.people, player_msg_install(room_state_changed, Room_info)),
			Room_info1 = set_win_info(set_hand_info(deal_all_board_card(reset_gamblers_title(fill_pot_and_reset_high_bet(change_game_state(Room_info, show_down)))))),
			dealing_show_down_time(Room_info1);
		deal_next_card ->
			dm_room:broadcast(Room_info#room_info.people, player_msg_install(room_state_changed, Room_info)),
			ask_active_gambler(set_hand_info(deal_board_card(prepare_active(reset_gamblers_title(fill_pot_and_reset_high_bet(change_game_state(Room_info, next_game_state(Room_info))))))));
		ask_active_gambler_ ->
			ask_active_gambler(set_next_active_position(Room_info));
		_ -> Room_info
	end.

check_room_condition(Room_info) ->
	Game_state  = Room_info#room_info.state,
	Gamblers 	= Room_info#room_info.gamblers,
	Fun  = fun (G, {{All_in, Fold, Action_done, Action_wait}, Num}) -> 
			case G of 
				[] -> {{All_in, Fold, Action_done, Action_wait}, Num}; 
				_ -> 
					State = G#gambler.state,
					Title = G#gambler.title, 
					case {Title, State} of
						{_, waiting_next_game}  -> {{All_in, Fold, Action_done, Action_wait}, Num};
						{big_blind, _} 	 		-> {{All_in, Fold, Action_done, Action_wait+1}, Num+1};
						{small_blind, _}  		-> {{All_in, Fold, Action_done, Action_wait+1}, Num+1};
						{name_or_last, _}		-> {{All_in, Fold, Action_done, Action_wait+1}, Num+1}; 
						{call, _} 		 		-> {{All_in, Fold, Action_done+1, Action_wait}, Num+1};
						{check, _} 		 		-> {{All_in, Fold, Action_done+1, Action_wait}, Num+1};
						{raise, _} 		 		-> {{All_in, Fold, Action_done+1, Action_wait}, Num+1};
						{fold, _} 		 		-> {{All_in, Fold+1, Action_done, Action_wait}, Num+1};	
						{all_in, _}       		-> {{All_in+1, Fold, Action_done, Action_wait}, Num+1};
						_						-> {{All_in, Fold, Action_done, Action_wait}, Num}
					end
			end 
		end,
	{State_statistics, Num} = lists:foldl(Fun, {{0, 0, 0, 0}, 0}, Gamblers),
	case State_statistics of
		{_, Fold, _, _} when Fold == Num-1 -> baffler_win;
		{_, _, 0, 0} -> to_show_down;
		{_, _, 1, 0} -> to_show_down;
		{_, _, _, 0} when Game_state == river -> to_show_down;
		{_, _, _, 0} -> deal_next_card;
		_ -> ask_active_gambler_
	end.
%%
%	Next_active     = next_active_position(Room_info, Room_info#room_info.active_position),
%	Last_active		= Room_info#room_info.round_over_position,
%	Room_info1	 	= Room_info#room_info{active_position=Next_active},
%	if Next_active == Last_active ->
%			room_new_state(Room_info1);
%		true ->
%			ask_active_gambler(Room_info1)
%	end,
%	Room_info1.

reset_gamblers_title(Room_info) ->
	Gamblers  = Room_info#room_info.gamblers,
	Fun = fun (G) -> case G of [] -> []; _ -> S=G#gambler.title, case S of all_in -> G; fold -> G; _ -> G#gambler{title=name_or_last} end end end,
	Gamblers2 = lists:map(Fun, Gamblers),
	Room_info#room_info{gamblers=Gamblers2}.

fill_pot_and_reset_high_bet(Room_info) ->
	Bets = Room_info#room_info.bets,
	Pots  = Room_info#room_info.pot,
	Fun  =  fun (Bet, {Pots1, Acc}) -> 
				Pot = lists:nth(Acc, Pots1),
				{0, {util:lists_set_nth(Acc, Pot+Bet, Pots1), Acc+1}} 
		    end,
	{Bets2, {Pots2, _}} = lists:mapfoldl(Fun, {Pots, 1}, Bets),
	Room_info#room_info{pot=Pots2, bets=Bets2, high_bet=0}.

next_active_position(Room_info, Position) ->
	Active_position = Position,
	Gamblers 		= util:rearrange_lists(Active_position, Room_info#room_info.gamblers),
	Fun   = fun (G, {Acc, Done}) -> 
				case {G, Done} of
					{_, true} -> {Acc, true};
					{[], false} -> {Acc+1, false};
					{_, false} -> 
						case {G#gambler.state, G#gambler.title} of
							{waiting_next_game, _} -> {Acc+1, false};
							{_, fold} -> {Acc+1, false};
							{_, all_in} -> {Acc+1, false};
							_ -> {Acc+1, true}
						end 
				end
			end,
	{Acc1, true}	= lists:foldl(Fun, {0, false}, Gamblers),
	((Active_position + Acc1 - 1) rem ?TOTAL_NUMBER_PLYAER) + 1.

room_call(Position, Room_info) ->
	Gamblers = Room_info#room_info.gamblers,
	Bets 	 = Room_info#room_info.bets,
	Bet 	 = lists:nth(Position, Bets),
	Gambler  = lists:nth(Position, Gamblers),
	High_bet = Room_info#room_info.high_bet,
	Diff 	 = High_bet - Bet,
	Buyin 	 = Gambler#gambler.buyin,
	if Buyin >= Diff ->
		Buyin2     = Buyin - Diff,
		Bet2       = High_bet,
		dm_room:to_player(Gambler#gambler.pid, player_msg_install(player_state_changed, {change_balance, -Diff})),
		Gambler2   = Gambler#gambler{buyin=Buyin2, state=waiting_room, title=call},
		Gamblers2  = util:lists_set_nth(Position, Gambler2, Gamblers),
		Bets2      = util:lists_set_nth(Position, Bet2, Bets),
		Room_info2 = after_action(Room_info#room_info{gamblers=Gamblers2, bets=Bets2}),
		dm_room:broadcast(Room_info2#room_info.people, player_msg_install(room_state_changed, Room_info2)),
		Room_info2;
	   true ->
			Room_info
	end.

room_all_in(Position, Room_info) ->
	High_bet   = Room_info#room_info.high_bet,
	Gamblers   = Room_info#room_info.gamblers,
	Bets 	   = Room_info#room_info.bets,
	Bet 	   = lists:nth(Position, Bets),
	Gambler    = lists:nth(Position, Gamblers),
	Diff       = Gambler#gambler.buyin,
	dm_room:to_player(Gambler#gambler.pid, player_msg_install(player_state_changed, {change_balance, -Diff})),
	Bet2       = Bet + Diff,
	Bets2      = util:lists_set_nth(Position, Bet2, Bets),
	%Gambler2   = Gambler#gambler{buyin=0, state=waiting_room, title=all_in},
	Fun = 	fun (G, Acc) -> 
						case G of 
							[] -> {[], Acc+1}; 
							_ ->
								case Acc of
									Position -> {G#gambler{state=waiting_room, title=all_in, buyin=0}, Acc+1};
									_ -> 
										Title = G#gambler.title,
										case Title of
											all_in -> {G, Acc+1};
											fold   -> {G, Acc+1};
											_ 	   -> {G#gambler{title=name_or_last}, Acc+1}
										end
								end   
						end
					end,
	{Gamblers2, _}  = lists:mapfoldl(Fun, 1, Gamblers),
	%Gamblers2  = util:lists_set_nth(Position, Gambler2, Gamblers),
	if Bet2>High_bet ->
			High_bet2 = Bet2;
	   true ->
	   		High_bet2 = High_bet
	end, 
	Room_info1 = after_action(Room_info#room_info{gamblers=Gamblers2,bets=Bets2,high_bet=High_bet2}),
	dm_room:broadcast(Room_info1#room_info.people, player_msg_install(room_state_changed, Room_info1)),
	Room_info1.

room_check(Position, Room_info) ->
	Gamblers = Room_info#room_info.gamblers,
	Bets 	 = Room_info#room_info.bets,
	Bet 	 = lists:nth(Position, Bets),
	Gambler  = lists:nth(Position, Gamblers),
	High_bet = Room_info#room_info.high_bet,
	if 	High_bet == Bet  ->
			Gambler2   = Gambler#gambler{state=waiting_room, title=check},
			Gamblers2  = util:lists_set_nth(Position, Gambler2, Gamblers),
			Room_info2 = after_action(Room_info#room_info{gamblers=Gamblers2}),
			dm_room:broadcast(Room_info2#room_info.people, player_msg_install(room_state_changed, Room_info2)),
			Room_info2;
	 	true ->
	 		Room_info
	end.

room_fold(Position, Room_info) ->
	Gamblers   = Room_info#room_info.gamblers,
	Gambler    = lists:nth(Position, Gamblers),
	Gambler2   = Gambler#gambler{state=waiting_room, title=fold},
	Gamblers2  = util:lists_set_nth(Position, Gambler2, Gamblers),
	Room_info2 = after_action(Room_info#room_info{gamblers=Gamblers2}),
	dm_room:broadcast(Room_info2#room_info.people, player_msg_install(room_state_changed, Room_info2)),
	Room_info2.

room_raise(Position, New_Bet, Room_info) ->
	High_bet = Room_info#room_info.high_bet,
	Big_Blin = Room_info#room_info.blind_bet,
	Gamblers = Room_info#room_info.gamblers,
	Bets 	 = Room_info#room_info.bets,
	Bet 	 = lists:nth(Position, Bets),
	Gambler  = lists:nth(Position, Gamblers),
	Buyin 	 = Gambler#gambler.buyin,
	Diff 	 = New_Bet - Bet,
	case is_valid_raise(Big_Blin, High_bet, Buyin, New_Bet, Diff) of
		true ->
			Bets2	 = util:lists_set_nth(Position, New_Bet, Bets),
			Buyin2   = Buyin - Diff,
			dm_room:to_player(Gambler#gambler.pid, player_msg_install(player_state_changed, {change_balance, -Diff})),
			Fun = 	fun (G, Acc) -> 
						case G of 
							[] -> {[], Acc+1}; 
							_ ->
								case Acc of
									Position -> {G#gambler{state=waiting_room, title=raise, buyin=Buyin2}, Acc+1};
									_ -> 
										Title = G#gambler.title,
										case Title of
											all_in -> {G, Acc+1};
											fold   -> {G, Acc+1};
											_ 	   -> {G#gambler{title=name_or_last}, Acc+1}
										end
								end   
						end
					end,
			{Gamblers2, _}  = lists:mapfoldl(Fun, 1, Gamblers),
			Room_info2 = after_action(Room_info#room_info{gamblers=Gamblers2, high_bet=New_Bet, bets=Bets2}),
			dm_room:broadcast(Room_info2#room_info.people, player_msg_install(room_state_changed, Room_info2)),
			Room_info2;
		_ ->
			Room_info
	end.

is_valid_raise(Big_Blin, High_bet, Buyin, New_Bet, Diff) ->
	case High_bet of
		0 -> Min_raise = Big_Blin;
		_ -> Min_raise = 2*High_bet
	end,
	case New_Bet of
		_ when New_Bet>=Min_raise, Diff<Buyin -> true;
		_ -> false
	end.

room_sit_down(Position, From, UID, Buyin, Room_info) ->
	Gamblers = Room_info#room_info.gamblers,
	Gambler  = lists:nth(Position, Gamblers),
	case Gambler of 
		[] ->
			Gambler1   = #gambler{pid=From, uid=UID, buyin=Buyin},
			Gamblers1  = util:lists_set_nth(Position, Gambler1, Gamblers),
			Room_info1 = Room_info#room_info{gamblers=Gamblers1},
			dm_room:to_player(From, player_msg_install(sit_down_success, Position)),
			Room_info2 = case Room_info#room_info.state of
                			ready ->
                				try_begin_game(Room_info1);
                			_ ->
								Room_info1                
                		 end,
			dm_room:broadcast(Room_info#room_info.people, player_msg_install(room_state_changed, Room_info2)),
			Room_info2;
		_ ->
			Room_info
	end.

try_begin_game(Room_info) ->
	Gamblers     = Room_info#room_info.gamblers,
	Gamblers2    = kick_losers(Gamblers),
	Num_Gamblers = lists:flatlength(Gamblers2),
	case Num_Gamblers of 
		0 ->
			clear_gamblers_cards(Room_info#room_info{state=ready, board=[], gamblers=Gamblers2});
		1 ->
			clear_gamblers_cards(Room_info#room_info{state=ready, board=[], gamblers=Gamblers2});
		2 ->
			begin_game(clear_gamblers_cards(Room_info#room_info{one_v_one=true, board=[], gamblers=Gamblers2}));
		_ ->
			begin_game(clear_gamblers_cards(Room_info#room_info{one_v_one=false, board=[], gamblers=Gamblers2}))
	end.

kick_losers(Gamblers) ->
	Fun = fun (G) -> 
			case G of 
				[] -> [];
				 _ -> 
				 	if G#gambler.buyin==0 -> 
				 		dm_room:to_player(G#gambler.pid, player_msg_install(kicked, oh_my_poor_guy)),
				 		[];
				 	  true ->  
				 	 	G 
				 	end 
			end
		 end,
	lists:map(Fun, Gamblers).

clear_gamblers_cards(Room_info) ->
	Gamblers  = Room_info#room_info.gamblers,
	Fun 	  = fun (G) -> case G of [] -> []; _ -> G#gambler{hand=[], hand_info=[]} end end,
	Gamblers2 = lists:map(Fun, Gamblers),
	Room_info#room_info{gamblers=Gamblers2}.

begin_game(Room_info) ->
	Room_info2 = reset_pot(reset_counter(prepare_active(change_game_state(deal_cards(shuffle_cards(prepare_blind(prepare_dealer(Room_info)))), pre_flop)))),
	ask_active_gambler(Room_info2).

ask_active_gambler(Room_info) ->
	Counter         = Room_info#room_info.counter,
	Active_position = Room_info#room_info.active_position,
	Gamblers 		= Room_info#room_info.gamblers,
	Gambler 		= lists:nth(Active_position, Gamblers),
	Gamblers2       = util:lists_set_nth(Active_position, Gambler#gambler{state=waiting_client}, Gamblers),
	Room_info2      = Room_info#room_info{gamblers=Gamblers2, counter=Counter+1},
	send_after(Room_info2, ?WAITING_TIME, Room_info2#room_info.counter).

reset_pot(Room_info) ->
	Room_info#room_info{pot=[0,0,0,0,0,0,0,0,0]}.

reset_counter(Room_info) ->
	Room_info#room_info{counter=0}.

change_game_state(Room_info, State) ->
	Room_info#room_info{state=State}.

next_game_state(Room_info) ->
	State = Room_info#room_info.state,
	case State of
		ready 	 -> pre_flop;
		pre_flop -> flop;
		flop 	 -> turn;
		turn 	 -> river;
		_        -> error
	end.

prepare_dealer(Room_info) ->
	Dealer_position_ = Room_info#room_info.dealer_position,
	Dealer_position  = Dealer_position_ rem ?TOTAL_NUMBER_PLYAER + 1,
	Gamblers1 		 = util:rearrange_lists(Dealer_position-1, Room_info#room_info.gamblers),
	Fun       		 = fun (G, {Acc, Done}) -> case {G, Done} of {_, true} -> {Acc, true}; {[], false} -> {Acc+1, false}; _ -> {Acc, true} end end,
	{Acc1, _} 		 = lists:foldl(Fun, {0, false}, Gamblers1),
	Dealer_position1 = (Dealer_position+Acc1-1) rem ?TOTAL_NUMBER_PLYAER + 1,
	%Gamblers2 = util:rearrange_lists(?TOTAL_NUMBER_PLYAER+1-Dealer_position, Gamblers1),
	Room_info#room_info{dealer_position = Dealer_position1}.

prepare_blind(Room_info) ->
	One_v_one 		= Room_info#room_info.one_v_one,
	Gamblers  		= Room_info#room_info.gamblers,
	Dealer_position = Room_info#room_info.dealer_position,
	case One_v_one of
		true ->
			Fun   = fun (G, {Acc, Bets}) -> 
						case {G, {Acc, Bets}} of 
							{[], _} -> {[], {Acc+1, Bets}}; 
							{_, {Dealer_position, _}}  -> 
								Pay   = Room_info#room_info.blind_bet div 2,
								Buyin = G#gambler.buyin,
								if 	Buyin >= Pay ->
										Diff = Pay;
									true ->
										Diff = Buyin
								end,
								dm_room:to_player(G#gambler.pid, player_msg_install(player_state_changed, {change_balance, -Diff})),
								{gambler_blind(G, small_blind, Diff), {Acc+1, util:lists_set_nth(Acc, Diff, Bets)}}; 
							_  -> 
								Pay   = Room_info#room_info.blind_bet,
								Buyin = G#gambler.buyin,
								if 	Buyin >= Pay ->
										Diff = Pay;
									true ->
										Diff = Buyin
								end,
								dm_room:to_player(G#gambler.pid, player_msg_install(player_state_changed, {change_balance, -Diff})),
								{gambler_blind(G, big_blind, Diff), {Acc+1,  util:lists_set_nth(Acc, Diff, Bets)}}
						end
				 	end,
			{Gamblers1, {_, Bets1}} = lists:mapfoldl(Fun, {1, Room_info#room_info.bets}, Gamblers),
			Room_info#room_info{gamblers=Gamblers1, bets=Bets1, high_bet=Room_info#room_info.blind_bet};
		_ -> 
			Gamblers1	 = util:rearrange_lists(Room_info#room_info.dealer_position, Gamblers),
			Fun 		 = 	fun (G, {Order, Intro}) -> 
								case G of 
									[] -> {Order, [[]| Intro]};
									_ ->
									    case {Order, G#gambler.state} of 
									    	{0, waiting_next_game} -> {1, [b| Intro]};
									    	{0, _} -> {1, [s| Intro]};
									    	{1, _} -> {2, [b| Intro]};
									    	{2, waiting_next_game} -> {2, [b| Intro]};
									    	_ -> {2, [[]| Intro]}
									    end
								end
							end,
			{_, Intro1}  = lists:foldl(Fun, {0, []}, Gamblers1),
			Intro2    	 = util:rearrange_lists(?TOTAL_NUMBER_PLYAER-Room_info#room_info.dealer_position, lists:reverse(Intro1)),
			Fun1         =  fun (G, {[H|T], Bets}) ->
								case {G, H} of
									{[], _} -> {[], {T, [0| Bets]}};
									{_, b} -> 
										Pay   = Room_info#room_info.blind_bet,
										Buyin = G#gambler.buyin,
										if 	Buyin >= Pay ->
												Diff = Pay;
											true ->
												Diff = Buyin
										end,
										dm_room:to_player(G#gambler.pid, player_msg_install(player_state_changed, {change_balance, -Diff})),
										{gambler_blind(G, big_blind, Diff), {T, [Diff| Bets]}};	
									{_, s} ->	
										Pay   = Room_info#room_info.blind_bet div 2,
										Buyin = G#gambler.buyin,
										if 	Buyin >= Pay ->
												Diff = Pay;
											true ->
												Diff = Buyin
										end,
										dm_room:to_player(G#gambler.pid, player_msg_install(player_state_changed, {change_balance, -Diff})),
										{gambler_blind(G, small_blind, Diff), {T, [Diff| Bets]}};
									_ ->
										{gambler_blind(G, name_or_last, 0), {T, [0| Bets]}}
								end
							end, 
			{Gamblers2, {_, Bets1}} = lists:mapfoldl(Fun1, {Intro2, []}, Gamblers),
			Room_info#room_info{gamblers=Gamblers2, bets=lists:reverse(Bets1), high_bet=Room_info#room_info.blind_bet}
	end.

gambler_blind(Gambler, Small_or_big, Diff) ->
	Buyin = Gambler#gambler.buyin,
	Gambler#gambler{state=waiting_room, title=Small_or_big, hand=[], buyin=Buyin-Diff}.

prepare_active(Room_info) ->
	One_v_one 		= Room_info#room_info.one_v_one,
	State 			= Room_info#room_info.state,
	Dealer_position = Room_info#room_info.dealer_position,
	case {One_v_one, State} of
		{true, pre_flop} ->
			Room_info#room_info{active_position = Dealer_position};
		{false, pre_flop} ->
			Next_active_position = next_active_position(Room_info, next_active_position(Room_info, next_active_position(Room_info, Room_info#room_info.dealer_position))),
			Room_info#room_info{active_position = Next_active_position};
		_->
			Next_active_position = next_active_position(Room_info, Room_info#room_info.dealer_position),
			Room_info#room_info{active_position = Next_active_position}
	end.

set_next_active_position(Room_info) ->
	Room_info#room_info{active_position = next_active_position(Room_info, Room_info#room_info.active_position)}.

shuffle_cards(Room_info) ->
	{Cards, _} = lists:split(2*?TOTAL_NUMBER_PLYAER+5, util:shuffle_lists(?POKER_CARDS)),
	Room_info#room_info{cards=Cards}.

deal_cards(Room_info) ->
	deal_cards_(Room_info, 2).

deal_cards_(Room_info, 0) ->
	Room_info;
deal_cards_(Room_info, Num) ->
	Cards    = Room_info#room_info.cards,
	Gamblers = Room_info#room_info.gamblers,
	Fun      =  fun (G, Acc) -> 
					case G of 
						[] ->
							{[],Acc}; 
						_ ->
							case {G#gambler.state, G#gambler.title} of
								{waiting_next_game, _} -> {G, Acc};
								{waiting_client, _} -> {G, Acc}; %% for useless safety.
								{_, fold} -> {G, Acc};
								{_, all_in} -> {G, Acc};
								_ ->
									[C1|L] = Acc,
									Hand1  = G#gambler.hand,							
									{G#gambler{hand=[C1|Hand1]}, L}
							end
					end
				end,
	{Gamblers1, Cards1} = lists:mapfoldl(Fun, Cards, Gamblers),
	deal_cards_(Room_info#room_info{cards=Cards1, gamblers=Gamblers1}, Num-1).

deal_board_card(Room_info) ->
	State = Room_info#room_info.state,
	Board = Room_info#room_info.board,
	Cards = Room_info#room_info.cards,
	case State of 
		flop -> 
			{Board2, Cards2} = lists:split(3, Cards);
		_ ->
			[Card2|Cards2] = Cards,
			Board2 = [Card2|Board]
	end,
	Room_info#room_info{cards=Cards2, board=Board2}.

deal_all_board_card(Room_info) ->
	Board 	  		 = Room_info#room_info.board,
	Cards 			 = Room_info#room_info.cards,
	Board_Num 	     = lists:foldl(fun (_,Acc) -> Acc+1 end, 0, Board),
	{Board2, Cards2} = deal_left_board_card(Board, Cards, 5-Board_Num ),
	Room_info#room_info{board=Board2, cards=Cards2}.

deal_left_board_card(Board, Cards, 0) ->
	{Board, Cards}; 
deal_left_board_card(Board, [Card1|Cards1], Num) ->
	deal_left_board_card([Card1|Board], Cards1, Num-1).

set_hand_info(Room_info) ->
	Board    = Room_info#room_info.board,
	Gamblers = Room_info#room_info.gamblers,
	Fun = fun (G) -> case G of [] -> G; _ -> Cards = G#gambler.hand, case Cards of [] -> G; _ -> G#gambler{hand_info=texas_poker_rule:check_hand(lists:append(Board, Cards))} end end end,
	Gamblers1 = lists:map(Fun, Gamblers),
	Room_info#room_info{gamblers = Gamblers1}.

set_baffler_win_info(Room_info) ->
	set_win_info(Room_info).

set_win_info(Room_info) ->
	Side_pool = side_pool(Room_info),
	Win_info  = compete_gamblers(Side_pool, Room_info),
	Room_info#room_info{win_info = Win_info}.

side_pool(Room_info) ->
	Side_pool  = side_pool_(Room_info),
	Unavailable_pos = check_out_unavailable_pos(Room_info),
	Side_pool1 = filter_unavailable_pos(Side_pool, Unavailable_pos),
	lists:reverse( merge_same_gamblers(Side_pool1) ).

compete_gamblers(Side_pool, Room_info) ->
	Gamblers = Room_info#room_info.gamblers,
	Fun = fun ({B, Ps}) ->  [B, Ps, winner_set(Ps, Gamblers)] end,
	lists:map(Fun, Side_pool).

winner_set(Ps, Gamblers) ->
	Hands 	= lists:map(fun (P) -> G = lists:nth(P, Gamblers), case G of [] -> {[], P}; _ -> {G#gambler.hand_info, P} end end, Ps),
	Hands1	= lists:reverse(lists:sort(fun ({H1, _}, {H2, _}) -> R = texas_poker_rule:compete_hand(H1, H2), if R > 0 -> false; true -> true end end, Hands)),
	[{H1,_}| _] = Hands1,
	lists:foldl(fun ({H, P}, Acc) ->  R = texas_poker_rule:compete_hand(H1, H), if R == 0 -> [P | Acc]; true -> Acc end end, [], Hands1).

check_out_unavailable_pos(Room_info) ->
	Gamblers = Room_info#room_info.gamblers,
	Fun  =	fun (G, {Acc, Pos}) -> 
				case G of 
					[] -> {Acc+1, [Acc| Pos]};
					_ -> 
						Title = G#gambler.title,
						State = G#gambler.state,
						case {Title, State} of 
							{_, waiting_next_game} -> {Acc+1, [Acc| Pos]};
							{fold, _} -> {Acc+1, [Acc| Pos]};
							_ ->	{Acc+1, Pos}
						end
				end
			end,
	{_, Pos} = lists:foldl(Fun, {1, []}, Gamblers),
	Pos.

filter_unavailable_pos(Side_pool, Unavailable_pos) ->
	Fun  =	fun ({A,Pos}) -> 
				Pos2 = lists:filter(fun (E) -> not lists:any(fun (E2) -> E2 == E end, Unavailable_pos) end, Pos),
				{A, Pos2}
			end,
	lists:map(Fun, Side_pool).

merge_same_gamblers([]) -> [];
merge_same_gamblers([S|[]]) -> [S];
merge_same_gamblers(Side_pool) ->
	[{B1 , Ps1}| R] = Side_pool,
	[{B2 , Ps2}| R2] = R,
	if 	Ps1 == Ps2 ->
			merge_same_gamblers([{B1+B2, Ps1}| R2]);
		true ->
			[{B1, Ps1} | merge_same_gamblers(R)]
	end.

side_pool_(Room_info) ->
	Pots  = Room_info#room_info.pot,
	{Pots_with_Position, _} = lists:mapfoldl(fun (Ele, Acc) -> {{Ele, Acc}, Acc+1} end, 1, Pots),
	Pots_with_Position_sorted_filtered = lists:filter(fun ({E, _}) -> E /= 0 end, lists:sort(Pots_with_Position)),
	set_one_side_pool(Pots_with_Position_sorted_filtered, 9).	

set_one_side_pool([], _) ->
	[];
set_one_side_pool(_, 0) ->
	[];
set_one_side_pool(Pots_with_Position_sorted_filtered, Num) ->
	[{Min, _}| _] = Pots_with_Position_sorted_filtered,
	Fun  = 	fun ({Bet, Position}, {Acc, Acc_p}) -> {{Bet-Min, Position}, {Acc+Min, [Position| Acc_p]}}	end,
	{Pots2, Side1} = lists:mapfoldl(Fun, {0,[]}, Pots_with_Position_sorted_filtered),
	[Side1| set_one_side_pool(trim_0_bet(Pots2), Num-1)].

trim_0_bet([]) -> [];
trim_0_bet(Pots) ->
	[{Bet, _} | L] = Pots,
	if 	Bet == 0 ->
			trim_0_bet(L);
		true ->
			Pots
	end.

room_stand_up(Position, From, Room_info) ->
	Gamblers = Room_info#room_info.gamblers,
	Gambler  = lists:nth(Position, Gamblers),
	case Gambler of 
		[] -> Room_info;
		_ ->
			Pid = Gambler#gambler.pid,
			case Pid of 
				From ->	
					Room_info1 = after_stand_up(Position, Gambler, Room_info#room_info{gamblers = util:lists_set_nth(Position,[],Gamblers)} ),
					dm_room:broadcast(Room_info1#room_info.people, player_msg_install(room_state_changed, Room_info1)),
					Room_info1;
				_ -> Room_info
			end
	end.

after_stand_up(Position, Gambler, Room_info) -> 
	GState = Gambler#gambler.state,
	GTitle = Gambler#gambler.title,
	RState = Room_info#room_info.state,
	case {GState, GTitle, RState} of
		{_, _, show_down} -> filter_win_info_after_stand_up(Position, Room_info);
		{waiting_next_game, _, _} -> Room_info;
		{waiting_client, _, _} -> after_stand_up(waiting_client,Room_info); 
		{waiting_room, fold, _} -> Room_info;
		{waiting_room, _, _} -> after_stand_up(waiting_room,Room_info)
	end.

after_stand_up(Stander_State, Room_info) ->
	Condition = check_room_condition(Room_info),
	case {Stander_State, Condition} of
		{_, baffler_win} ->
			dm_room:broadcast(Room_info#room_info.people, player_msg_install(room_state_changed, Room_info)),
			Room_info1 = set_baffler_win_info(set_hand_info(reset_gamblers_title(fill_pot_and_reset_high_bet(change_game_state(Room_info, show_down))))),
			dealing_show_down_time(Room_info1);
		{_, to_show_down} ->
			dm_room:broadcast(Room_info#room_info.people, player_msg_install(room_state_changed, Room_info)),
			Room_info1 = set_win_info(set_hand_info(deal_all_board_card(reset_gamblers_title(fill_pot_and_reset_high_bet(change_game_state(Room_info, show_down)))))),
			dealing_show_down_time(Room_info1);
		{_, deal_next_card} ->
			dm_room:broadcast(Room_info#room_info.people, player_msg_install(room_state_changed, Room_info)),
			ask_active_gambler(set_hand_info(deal_board_card(prepare_active(reset_gamblers_title(fill_pot_and_reset_high_bet(change_game_state(Room_info, next_game_state(Room_info))))))));
		{waiting_room, ask_active_gambler_} ->
			Room_info;
		{waiting_client, ask_active_gambler_} ->
			ask_active_gambler(set_next_active_position(Room_info));
		_ -> 
			Room_info
	end.

filter_win_info_after_stand_up(Position, Room_info) ->
	Win_info  = Room_info#room_info.win_info,
	Fun  = fun ([Bet, Ps, Ws]) -> [Bet, lists:delete(Position, Ps), lists:delete(Position, Ws)] end,
	Win_info1 = lists:map(Fun, Win_info),
	%Fun2 = fun (E) -> case E of [_, _, []] -> true; _ -> false end end,
	%Win_info2 = lists:filter(Fun2, Win_info1),
	%Room_info#room_info{win_info=Win_info2}.
	Room_info#room_info{win_info=Win_info1}.

cancel_timer(Room_info) ->
	Timer = Room_info#room_info.timer,
	case Timer of 
		undefined -> false;
		_ -> erlang:cancel_timer(Timer)
	end.

send_after(Room_info, Seconds, Msg) ->
	cancel_timer(Room_info),
	Room_info#room_info{timer = dm_room:send_after(Seconds, {wait_time_out, Msg})}.

player_msg_install(Api, {Content,Extra}) ->
	{[{?API2, Api}, {?CONTENT2, Content}, {?EXTRA, Extra}]};
player_msg_install(Api, Content) ->
	{[{?API2, Api}, {?CONTENT2, Content}]}.

test() ->
	R = merge_same_gamblers([{23,[1,2,3,4]},{34,[1,2,3,4]},{44,[2,3,4]},{54,[2,3]}]),
	R.

