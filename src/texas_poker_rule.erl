-module(texas_poker_rule).

%% I have to admit this implement is ineffective.

-export([check_hand/1, compete_hand/2, test/0]).

%% Card Structure : [[Type, Number] || Type <- [s, h ,d ,c] , Number <- lists:seq(2, 14)]
%% Result Structure : [Result, Hand] || Result <- lists:seq(1, 10) , Hand <- [Card1,Card2,Card3,Card4,Card5]

-define(ROYAL_FLUSH, 10).
-define(STRAIGHT_FLUSH, 9).
-define(FOUR_OF_A_KIND, 8).
-define(FULL_HOUSE, 7).
-define(FLUSH, 6).
-define(STRAIGHT, 5).
-define(THREE_OF_A_KIND, 4).
-define(TWO_PAIRS, 3).
-define(PAIR, 2).
-define(HIGH_CARD, 1).

compete_hand([], []) -> 0;
compete_hand([], _) -> -1;
compete_hand(_, []) -> 1;
compete_hand(Result1, Result2) ->
	[Rank1, Hand1] = Result1,
	[Rank2, Hand2] = Result2,
	case Rank1-Rank2 of 
		R when R > 0 ->
			1;
		R when R < 0 ->
			-1;
		_ ->
			compete_number(Hand1, Hand2)
	end.	

compete_number([], []) ->
	0;
compete_number([H1|Hand1], [H2|Hand2]) ->
	[_, N1] = H1,
	[_, N2] = H2,
	case N1-N2 of 
		N when N > 0 ->
			1;
		N when N < 0 ->
			-1;
		_ ->
			compete_number(Hand1, Hand2)
	end.


lists2tuple(Cards) ->
	Fun = fun ([T,N]) -> {T,N} end,
	lists:map(Fun, Cards).

tuple2lists([]) -> [];
tuple2lists(Tuple2) ->
	Fun = fun ({T,N}) -> [T,N] end,
	lists:map(Fun, Tuple2).

check_hand(Cards) ->
	check_hand(tuple2lists(lists:reverse(lists:keysort(2, lists2tuple(Cards)))), 10, 
	   [fun is_royal_flush/1, 
		fun is_straight_flush/1,
		fun is_four_of_a_kind/1,
		fun is_full_house/1,
		fun is_flush/1,
		fun is_straight/1,
		fun is_three_of_a_kind/1,
		fun is_two_pairs/1,
		fun is_pair/1,
	 	fun is_high_card/1]).

check_hand(Cards, N, [Fun | Funs]) ->
	[Rank, Hand] = Fun(Cards),
	case Rank of 
		_ when Rank > 0 ->	[N, Hand];
		_ -> check_hand(Cards, N-1, Funs)
	end.

seven_to_five([C1,C2,C3,C4,C5,C6,C7]) ->
	[[C1,C2,C3,C4,C5], [C1,C2,C3,C4,C6], [C1,C2,C3,C4,C7],
	 [C1,C2,C3,C5,C6], [C1,C2,C3,C5,C7], [C1,C2,C3,C6,C7],
	 [C1,C2,C4,C5,C6], [C1,C2,C4,C5,C7], [C1,C2,C4,C6,C7],
	 [C1,C2,C5,C6,C7], [C1,C3,C4,C5,C6], [C1,C3,C4,C5,C7],
	 [C1,C3,C4,C6,C7], [C1,C3,C5,C6,C7], [C1,C4,C5,C6,C7], 
	 [C2,C3,C4,C5,C6], [C2,C3,C4,C5,C7], [C2,C3,C4,C6,C7], 
	 [C2,C3,C5,C6,C7], [C2,C4,C5,C6,C7], [C3,C4,C5,C6,C7]].

six_to_five([C1,C2,C3,C4,C5,C6]) ->
	[[C1,C2,C3,C4,C5], [C1,C2,C3,C4,C6], [C1,C2,C3,C5,C6],
	 [C1,C2,C4,C5,C6], [C1,C3,C4,C5,C6], [C2,C3,C4,C5,C6]].

five_to_five(C) -> [C].

is_royal_flush(Cards) ->
	is_royal_flush_(case lists:flatlength(Cards) of 14 -> seven_to_five(Cards); 12 -> six_to_five(Cards); 10 -> five_to_five(Cards); _ -> [] end).

is_royal_flush_([]) ->
	[0, []];
is_royal_flush_([H|Tail]) ->
	case H of 
		[[Type, 14], [Type, 13], [Type, 12], [Type, 11], [Type, 10]] ->
			[?ROYAL_FLUSH, [[Type, 14], [Type, 13], [Type, 12], [Type, 11], [Type, 10]]];
		_ ->
			is_royal_flush_(Tail)
	end.

is_straight_flush(Cards) ->
	is_straight_flush_(case lists:flatlength(Cards) of 14 -> seven_to_five(Cards); 12 -> six_to_five(Cards); 10 -> five_to_five(Cards); _ -> [] end).

is_straight_flush_([]) ->
	[0, []];
is_straight_flush_([H|Tail]) ->
	case H of 
		[[Type, N1], [Type, N2], [Type, N3], [Type, N4], [Type, N5]] when N1 == N2+1 , N2 == N3+1 , N3 == N4+1 , N4 == N5+1 ->
			[?STRAIGHT_FLUSH, H];
		[[Type, 14], [Type, 5], [Type, 4], [Type, 3], [Type, 2]] ->
			[?STRAIGHT_FLUSH, [[Type, 5], [Type, 4], [Type, 3], [Type, 2], [Type, 14]]];
		_ ->
			is_straight_flush_(Tail)
	end.

is_four_of_a_kind(Cards) ->
	is_four_of_a_kind_(case lists:flatlength(Cards) of 14 -> seven_to_five(Cards); 12 -> six_to_five(Cards); 10 -> five_to_five(Cards); _ -> [] end).

is_four_of_a_kind_([]) ->
	[0, []];
is_four_of_a_kind_([H|Tail]) ->
	case H of 
		[[_, N], [_, N], [_, N], [_, N], _] ->
			[?FOUR_OF_A_KIND, H];
		[A, [_, N], [_, N], [_, N], [_, N]] ->
			[?FOUR_OF_A_KIND, [[s, N], [h, N], [d, N], [c, N], A]];
		_ ->
			is_four_of_a_kind_(Tail)
	end.

is_full_house(Cards) ->
	is_full_house_(case lists:flatlength(Cards) of 14 -> seven_to_five(Cards); 12 -> six_to_five(Cards); 10 -> five_to_five(Cards); _ -> [] end).

is_full_house_([]) ->
	[0, []];
is_full_house_([H|Tail]) ->
	case H of 
		[[_, N], [_, N], [_, N], [_, N2], [_, N2]] ->
			[?FULL_HOUSE, H];
		[[T1, N2], [T2, N2], [T3, N], [T4, N], [T5, N]] ->
			[?FULL_HOUSE, [[T3, N], [T4, N], [T5, N], [T1, N2], [T2, N2]]];
		_ ->
			is_full_house_(Tail)
	end.

is_flush(Cards) ->
	is_flush_(case lists:flatlength(Cards) of 14 -> seven_to_five(Cards); 12 -> six_to_five(Cards); 10 -> five_to_five(Cards); _ -> [] end).

is_flush_([]) ->
	[0, []];
is_flush_([H|Tail]) ->
	case H of 
		[[T, _], [T, _], [T, _], [T, _], [T, _]] ->
			[?FLUSH, H];
		_ ->
			is_flush_(Tail)
	end.

is_straight(Cards) ->
	is_straight_(case lists:flatlength(Cards) of 14 -> seven_to_five(Cards); 12 -> six_to_five(Cards); 10 -> five_to_five(Cards); _ -> [] end).

is_straight_([]) ->
	[0, []];
is_straight_([H|Tail]) ->
	case H of 
		[[_, N1], [_, N2], [_, N3], [_, N4], [_, N5]] when N1 == N2+1 , N2 == N3+1 , N3 == N4+1 , N4 == N5+1 ->
			[?STRAIGHT, H];
		[[T1, 14], [T2, 5], [T3, 4], [T4, 3], [T, 2]] ->
			[?STRAIGHT, [[T2, 5], [T3, 4], [T4, 3], [T, 2], [T1, 14]]];
		_ ->
			is_straight_(Tail)
	end.

is_three_of_a_kind(Cards) ->
	is_three_of_a_kind_(case lists:flatlength(Cards) of 14 -> seven_to_five(Cards); 12 -> six_to_five(Cards); 10 -> five_to_five(Cards); _ -> [] end).

is_three_of_a_kind_([]) ->
	[0, []];
is_three_of_a_kind_([H|Tail]) ->
	case H of 
		[[_, N], [_, N], [_, N], _, _] ->
			[?THREE_OF_A_KIND, H];
		[A, [T1, N], [T2, N], [T3, N], B] ->
			[?THREE_OF_A_KIND, [[T1, N], [T2, N], [T3, N], A, B]];
		[A, B, [T1, N], [T2, N], [T3, N]] ->
			[?THREE_OF_A_KIND, [[T1, N], [T2, N], [T3, N], A, B]];
		_ ->
			is_three_of_a_kind_(Tail)
	end.

is_two_pairs(Cards) ->
	is_two_pairs_(case lists:flatlength(Cards) of 14 -> seven_to_five(Cards); 12 -> six_to_five(Cards); 10 -> five_to_five(Cards); _ -> [] end).

is_two_pairs_([]) ->
	[0, []];
is_two_pairs_([H|Tail]) ->
	case H of 
		[[_, N1], [_, N1], [_, N2], [_, N2], _] ->
			[?TWO_PAIRS, H];
		[[T1, N1], [T2, N1], A, [T3, N2], [T4, N2]] ->
			[?TWO_PAIRS, [[T1, N1], [T2, N1], [T3, N2], [T4, N2], A]];
		[A, [T1, N1], [T2, N1], [T3, N2], [T4, N2]] ->
			[?TWO_PAIRS, [[T1, N1], [T2, N1], [T3, N2], [T4, N2], A]];
		_ ->
			is_two_pairs_(Tail)
	end.

is_pair(Cards) ->
	is_pair_(case lists:flatlength(Cards) of 14 -> seven_to_five(Cards); 12 -> six_to_five(Cards); 10 -> five_to_five(Cards); _ -> [] end).

is_pair_([]) ->
	[0, []];
is_pair_([H|Tail]) ->
	case H of 
		[[_, N1], [_, N1], _, _, _] ->
			[?TWO_PAIRS, H];
		[A, [T1, N1], [T2, N1], B, C] ->
			[?TWO_PAIRS, [[T1, N1], [T2, N1], A, B, C]];
		[A, B, [T1, N1], [T2, N1], C] ->
			[?TWO_PAIRS, [[T1, N1], [T2, N1], A, B, C]];
		[A, B, C, [T1, N1], [T2, N1]] ->
			[?TWO_PAIRS, [[T1, N1], [T2, N1], A, B, C]];
		_ ->
			is_pair_(Tail)
	end.

is_high_card(Cards) ->
	is_high_card_(case lists:flatlength(Cards) of 14 -> seven_to_five(Cards); 12 -> six_to_five(Cards); 10 -> five_to_five(Cards); _ -> [] end).

is_high_card_([]) ->
	[1, []];
is_high_card_([H|_]) ->
	[?HIGH_CARD, H].

test() ->
	check_hand([[s,10],[s,11],[s,12],[s,13],[s,14]]).
