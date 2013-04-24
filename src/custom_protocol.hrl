
-define(GAME_TYPE_TO_MOD(Type), case Type of <<"normal">> -> normal_texas_poker; _ -> undefined end ).

-define(POKER_CARDS, [{Type, Number} || Type <- [s, h ,d ,c] , Number <- lists:seq(2, 14)]).

% basic protocol.
-define(API2, <<"a">>).
-define(CONTENT2, <<"c">>).
-define(EXTRA, <<"e">>).
-define(EXTRA2, <<"e2">>).

% message to client
-define(MESSAGE_SOMEONE_SIT_DOWN_SUCCESS(Msg1, Msg2), [<<"{\"a\": \"someone_sit_down_succcess\",">>, <<"\"c\": ">>, Msg1, <<"\"e\": ">>, Msg2,<<"}">>]).
-define(MESSAGE_I_SIT_DOWN_SUCCESS(Msg1, Msg2), [<<"{\"a\": \"i_sit_down_succcess\",">>, <<"\"c\": ">>, Msg1, <<"\"e\": ">>, Msg2,<<"}">>]).
-define(MESSAGE_GAME_BEGIN(Msg), [<<"{\"a\": \"game_begin\" ,">>, <<"\"c\": ">>, Msg, <<"}">>]).


% Players Behaviours in rooms.
-define(SIT_DOWN, <<"sd">>).
-define(STAND_UP,  <<"su">>).
-define(LEAVE_ROOM, <<"l">>).
-define(RAISE, <<"r">>).
-define(FOLD, <<"f">>).
-define(CHECK, <<"c">>).
-define(ALL_IN, <<"a">>).

% People Behaviours in rooms.
-define(TALK , <<"t">>).
