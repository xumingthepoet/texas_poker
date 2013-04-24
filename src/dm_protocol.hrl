%%=========================================================
%% Server Internal Protocols
%%=========================================================

-define(ENTER_ROOM_SUCCESS, enter_room_success).
-define(ENTER_ROOM_REQUEST, enter_room_request).

-define(HEART_BEAT, heart_beat).
-define(CAST_CLIENT, cast_client).
-define(RECONNECT_IDLE_PLAYER, idle_player).
-define(RECONNECT_BUSY_PLAYER, busy_player).
-define(RECONNECT_ERROR_PLAYER, error_player).

-define(SEND_MSG_TO_CLIENT, send_msg_to_clent).
-define(PLAYER_PROCESS_TERMINATED, player_process_terminated). 
-define(RECONNECT_PLAYER, reconnect_player).
-define(RECONNECT_TO_OTHER_TCP, reconnect_to_other_tcp).
-define(TCP_CLOSED, tcp_closed).
-define(TCP_TERMINATED, tcp_terminated).

-define(TCP_MODULE_NAME, dm_tcp).
-define(PLAYER_MODULE_NAME, dm_player).
-define(ROOM_MODULE_NAME, dm_room).

-define(GLOBAL_PLAYER_PROCESS_NAME(ID), {p, ID}).

%% dm_player  
-record(game_info, {room_id, room_mod, info}).

%%=========================================================
%% Request Api from Client
%%=========================================================

-define(API, <<"a">>).
-define(CONTENT, <<"c">>).

-define(LOGIN, <<"l">>).
-define(LOGOUT, <<"lo">>).
-define(ENTER_ROOM, <<"e">>).
-define(GAME_PROTOCOL, <<"gp">>).
-define(PROFILE_PROTOCOL, <<"pp">>).
-define(SOCIAL_PROTOCOL, <<"sp">>).

%%=========================================================
%% Binary Json Iolists to Client 
%%=========================================================

-define(MESSAGE_TCP_CONNECTED, [<<"{\"a\": \"tcp_connected\"}">>]).
-define(MESSAGE_LOGIN_SUCCESS, [<<"{\"a\": \"login_success\"}">>]).
-define(MESSAGE_LOGIN_FAILED, [<<"{\"a\": \"login_failed\"}">>]).
-define(MESSAGE_RECONNECT_SUCCESS, [<<"{\"a\": \"reconnect_success\"}">>]).
-define(MESSAGE_RECONNECT_SUCCESS_2(Msg), [<<"{\"a\": \"reconnect_success_2\",">>, <<"\"c\": ">>, Msg, <<"}">>]).
-define(MESSAGE_RECONNECT_FAILED, [<<"{\"a\": \"reconnect_failed\"}">>]).
-define(MESSAGE_ENTER_ROOM_SUCCESS(Msg), [<<"{\"a\": \"enter_room_succcess\",">>, <<"\"c\": ">>, Msg, <<"}">>]).
