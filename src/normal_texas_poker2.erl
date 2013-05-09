
-module(normal_texas_poker2).

-include("custom_protocol.hrl").
-include("dm_database.hrl").

-behaviour(gen_game).

-export([on_player_message/5, init_room/0, on_room_message/3, info2client/2, enter_room_request/2, leave_room/3]).

%% game_info for Player process, position <- lists:seq(0,9). if position == 0 , the player is standup.
-record(game_info, {position, room_info}).

%% room_info for Room process, cards is ?POKER_CARDS , peolple is Player process that watching , 
%% state <- [ready, pre_flop, flop, true, river, show_down ] , active_ or dealer_position <- lists:seq(1,9).
-record(room_info, {cards, people, counter, timer, active_position, 
		one_v_one, state, blind_bet, board, pot, high_bet, dealer_position, bets, gamblers}).

%% gamebler is real player model in game , state <- [waiting_next_game, waiting_room, waiting_client ...], 
%% title <- [name_or_last, small_blind, big_blind, call, raise, check, fold, all_in ], raised <- [true, false]
-record(gambler, {pid, uid, state=waiting_next_game, has_raised=false, title=name_or_last, buyin, hand=[]}).

-define(TOTAL_NUMBER_PLYAER, 9).
-define(WAITING_TIME, 5).

init_room() ->
	#room_info{cards = [], people = [], counter = 0, timer = undefined, one_v_one = false,  state = ready, blind_bet = 10,
	 board = [], pot = 0, high_bet = 0, dealer_position = 1, active_position = 1,
	 bets = [0, 0, 0, 0, 0, 0, 0, 0, 0], gamblers = [[], [], [], [], [], [], [], [], []]}.

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

	Json 	  = dm_protocol:json_new([{uid,Uid},{money,Money},{position,Position},{counter,Counter},{ovo,One_v_one},{state,State},{blind,Blind_bet},{board,Board},{pot,Pot},
				 {high_bet,High_bet},{dealer_position,D_p},{bets,Bets},{gamblers,gamblers_to_json(Gamblers)}]),
	
	dm_protocol:encode(Json).

protect_privacy_cards(Position, State , Gamblers) ->
	case {Position, State} of
		{_, show_down} ->	Gamblers2 = Gamblers;
		{0, _} -> Gamblers2 = lists:map(fun (G) -> case G of [] -> []; _ -> G#gambler{hand = []} end end, Gamblers);
		_ -> 
			Fun = fun(G, Acc) -> case Acc of Position -> {G, Acc+1}; _ -> case G of [] -> {[], Acc+1}; _ -> {G#gambler{hand = []}, Acc+1} end end end,
			{Gamblers2, _} = lists:mapfoldl(Fun, 1, Gamblers) 
	end,
	Gamblers2.

gamblers_to_json(Gamblers) ->
	lists:map(fun (G) -> case G of [] -> []; _ -> [G#gambler.state, G#gambler.has_raised, G#gambler.title, G#gambler.buyin, G#gambler.hand] end end, Gamblers).

on_player_message(Tcp, Player_info, Room_pid, Game_info, Msg) ->
	Api     = dm_protocol:json_get(?API2, Msg),
	Content = dm_protocol:json_get(?CONTENT2, Msg),
	Extra   = dm_protocol:json_get(?EXTRA, Msg),
	Extra2  = dm_protocol:json_get(?EXTRA2, Msg),
	case {Api, {Content, Extra, Extra2}} of
		{?LEAVE_ROOM, _} ->
			dm_player:to_room(Room_pid, {?LEAVE_ROOM, Game_info}),
			{Player_info, undefined};
		{player_state_changed, {change_balance, Diff, _}} ->
			dm_database:add_player_money(Player_info#player_info.uid, Diff),
			NewBalance = Player_info#player_info.money + Diff,
			%dm_player:to_client(Tcp, ?MESSAGE_PLAYER_BALANCE_CHANGED(integer_to_list(NewBalance))),
			{Player_info#player_info{money = NewBalance}, Game_info};
		{room_state_changed, {Room_info, _, _}} ->
			Game_info2 = Game_info#game_info{room_info = Room_info},
			dm_player:to_client(Tcp, ?MESSAGE_GAME_STATE_CHANGE(info2client(Player_info, Game_info2))),
			{Player_info, Game_info2};
		{?SIT_DOWN, {Position, Buyin, _}} ->
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
		{?STAND_UP, {Position, _, _}} ->
			dm_player:to_room(Room_pid, {?STAND_UP, Position}),
			{Player_info, Game_info#game_info{position=0}};
		{?RAISE, {Position, Counter, Times_of_raise}} ->
			dm_player:to_room(Room_pid, {?RAISE, Position, Counter, Times_of_raise}),
			{Player_info, Game_info};
		{?FOLD, {Position, Counter, _}} ->
			dm_player:to_room(Room_pid, {?FOLD, Position, Counter}),
			{Player_info, Game_info};
		{?CALL, {Position, Counter, _}} ->
			dm_player:to_room(Room_pid, {?CALL, Position, Counter}),
			{Player_info, Game_info};
		{?CHECK, {Position, Counter, _}} ->
			dm_player:to_room(Room_pid, {?CHECK, Position, Counter}),
			{Player_info, Game_info};
		{?ALL_IN, {Position, Counter, _}} ->
			dm_player:to_room(Room_pid, {?ALL_IN, Position, Counter}),
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
			(From == Gambler#gambler.pid) and (waiting_client == Gambler#gambler.state);
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
		{?RAISE, Position, Counter, Times}  ->
			case is_counter_and_position_match(Counter, Position, From, Room_info) of
				true ->	Room_info;
				_ -> Room_info
			end;
		{?FOLD, Position, Counter} ->
			case is_counter_and_position_match(Counter, Position, From, Room_info) of
				true ->	Room_info;
				_ -> Room_info
			end;
		{?CALL, Position, Counter} ->
			case is_counter_and_position_match(Counter, Position, From, Room_info) of
				true ->	room_call(Position, Room_info);
				_ -> Room_info
			end;
		{?CHECK, Position, Counter} ->
			case is_counter_and_position_match(Counter, Position, From, Room_info) of
				true ->	Room_info;
				_ -> Room_info
			end;
		{?ALL_IN, Position, Counter} ->
			case is_counter_and_position_match(Counter, Position, From, Room_info) of
				true ->	Room_info;
				_ -> Room_info
			end;
		_ ->
			Room_info
	end.

room_wait_time_out(Room_info) ->
	Active_position = Room_info#room_info.active_position,
	Gamblers        = Room_info#room_info.gamblers,
	Gambler         = lists:nth(Active_position, Gamblers),
	High_bet        = Room_info#room_info.high_bet,
	Gambler_bet     = lists:nth(Active_position, Room_info#room_info.bets),
	if High_bet > Gambler_bet ->
		   Gambler1 = Gambler#gambler{title = fold, state = waiting_room};
		true ->
		   Gambler1 = Gambler#gambler{title = check, state = waiting_room}
	end,
	Room_info1		= Room_info#room_info{gamblers = util:lists_set_nth(Active_position, Gambler1, Gamblers)},
	Room_info2      = after_action(Room_info1),
	dm_room:broadcast(Room_info#room_info.people, player_msg_install(room_state_changed, Room_info2)),
	Room_info2.

after_action(Room_info) ->
	Condition = check_room_condition(Room_info),
	case Condition of
		buffler_win ->
			Room_info;
		to_show_down -> 
			Room_info;
		deal_next_card ->
			dm_room:broadcast(Room_info#room_info.people, player_msg_install(room_state_changed, Room_info)),
			timer:sleep(500),
			ask_active_gambler(deal_board_card(prepare_active(reset_gamblers_title_and_raised(fill_pot_and_reset_high_bet(change_game_state(Room_info, next_game_state(Room_info)))))));
		ask_active_gambler_ ->
			ask_active_gambler(set_next_active_position(Room_info));
		_ -> Room_info
	end.
	
check_room_condition(Room_info) ->
	Gamblers = Room_info#room_info.gamblers,
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
		{_, Fold, _, _} when Fold == Num-1 -> buffler_win;
		{_, _, 0, 0} -> to_show_down;
		{_, _, 1, 0} -> to_show_down;
		{_, _, _, 0}-> deal_next_card;
		_ -> ask_active_gambler_
	end.

after_stand_up(Room_info) -> %% TODO: after_stand_up
	Room_info.
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
	
reset_gamblers_title_and_raised(Room_info) ->
	Gamblers  = Room_info#room_info.gamblers,
	Fun = fun (G) -> case G of [] -> []; _ -> S=G#gambler.title, case S of all_in -> G; fold -> G; _ -> G#gambler{title=name_or_last,has_raised=false} end end end,
	Gamblers2 = lists:map(Fun, Gamblers),
	Room_info#room_info{gamblers=Gamblers2}.

fill_pot_and_reset_high_bet(Room_info) ->
	Bets = Room_info#room_info.bets,
	Pot  = Room_info#room_info.pot,
	Fun  = fun (B, P) -> {0, B+P} end,
	{Bets2, Pot2} = lists:mapfoldl(Fun, Pot, Bets),
	Room_info#room_info{pot=Pot2, bets=Bets2, high_bet=0}.

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
	Num_Gamblers = lists:flatlength(Gamblers),
	case Num_Gamblers of 
		0 ->
			Room_info;
		1 ->
			Room_info;
		2 ->
			begin_game(Room_info#room_info{one_v_one=true});
		_ ->
			begin_game(Room_info#room_info{one_v_one=false})
	end.

begin_game(Room_info) ->
	Room_info2 = reset_counter(prepare_active(change_game_state(deal_cards(shuffle_cards(prepare_blind(prepare_dealer(Room_info)))), pre_flop))),
	Room_info3 = ask_active_gambler(Room_info2).

ask_active_gambler(Room_info) ->
	Counter         = Room_info#room_info.counter,
	Active_position = Room_info#room_info.active_position,
	Gamblers 		= Room_info#room_info.gamblers,
	Gambler 		= lists:nth(Active_position, Gamblers),
	Gamblers2       = util:lists_set_nth(Active_position, Gambler#gambler{state=waiting_client}, Gamblers),
	Room_info2      = Room_info#room_info{gamblers=Gamblers2, counter=Counter+1},
	send_after(Room_info2, ?WAITING_TIME, Room_info2#room_info.counter).

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
		river	 -> show_down;
		_        -> error
	end.

prepare_dealer(Room_info) ->
	Dealer_position  = Room_info#room_info.dealer_position,
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
			Gamblers1				 = util:rearrange_lists(Room_info#room_info.dealer_position, Gamblers),
			Fun 					 = 	fun (G, {Order, Intro}) -> 
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
			{_, Intro1}  			= lists:foldl(Fun, {0, []}, Gamblers1),
			Intro2    	 			= util:rearrange_lists(?TOTAL_NUMBER_PLYAER-Room_info#room_info.dealer_position, lists:reverse(Intro1)),
			Fun1                    =  	fun (G, {[H|T], Bets}) ->
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
	Gambler#gambler{state=waiting_room, has_raised=false, title=Small_or_big, hand=[], buyin=Buyin-Diff}.

prepare_active(Room_info) ->
	One_v_one 		= Room_info#room_info.one_v_one,
	State 			= Room_info#room_info.state,
	Gamblers  		= Room_info#room_info.gamblers,
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
	Room_info#room_info{cards=util:shuffle_lists(?POKER_CARDS)}.

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
			[Card1|Cards1] = Cards,
			Board1 = [Card1|Board], 
			[Card2|Cards2] = Cards1,
			Board2 = [Card2|Board1];
		_ ->
			[Card2|Cards2] = Cards,
			Board2 = [Card2|Board]
	end,
	Room_info#room_info{cards=Cards2, board=Board2}.

room_stand_up(Position, From, Room_info) ->
	Gamblers = Room_info#room_info.gamblers,
	Gambler  = lists:nth(Position, Gamblers),
	case Gambler of 
		[] -> Room_info;
		_ ->
			Pid = Gambler#gambler.pid,
			case Pid of 
				From ->	
					Room_info1 = after_stand_up( Room_info#room_info{gamblers = util:lists_set_nth(Position,[],Gamblers)} ),
					dm_room:broadcast(Room_info1#room_info.people, player_msg_install(room_state_changed, Room_info1)),
					Room_info1;
				_ -> Room_info
			end
	end. 

%%  

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


