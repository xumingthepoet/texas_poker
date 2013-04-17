%%=========================================================
%% Server Internal Protocols
%%=========================================================

-define(HEART_BEAT, heart_beat).
-define(CAST_CLIENT, cast_client).
-define(IDLE_PLAYER, idle_player).
-define(BUSY_PLAYER, busy_player).

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

-record(player_info, {uid, info}).
-record(game_info, {room_id, room_mod, info}).

%%=========================================================
%% Request Api from Client
%%=========================================================

-define(API, <<"api">>).
-define(LOGIN, <<"login">>).
-define(LOGOUT, <<"logout">>).
-define(ENTER_ROOM, <<"enter_room">>).
-define(GAME_TYPE, <<"game_type">>).
-define(UID, <<"uid">>).
-define(GAME_PROTOCOL, <<"game_protocol">>).
-define(GAME_ACTION, <<"game_action">>).

%%=========================================================
%% Binary Json Iolists to Client 
%%=========================================================

-define(MESSAGE_TCP_CONNECTED, [<<"{\"api\": \"tcp_connected\"}">>]).
-define(MESSAGE_LOGIN_SUCCESS, [<<"{\"api\": \"login_success\"}">>]).
-define(MESSAGE_LOGIN_FAILED, [<<"{\"api\": \"login_failed\"}">>]).
-define(MESSAGE_RECONNECT_SUCCESS, [<<"{\"api\": \"reconnect_success\"}">>]).
-define(MESSAGE_RECONNECT_SUCCESS_2(Msg), [<<"{\"api\": \"reconnect_success_2\",">>, <<"\"info\": ">>, Msg, "}"]).
-define(MESSAGE_RECONNECT_FAILED, [<<"{\"api\": \"reconnect_failed\"}">>]).