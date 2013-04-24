-module(dm_player).

-include("dm_protocol.hrl").
-include("dm_database.hrl").

-behaviour(gen_server).

-export([start_link/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([to_client/2, to_peer/2, to_room/2]).

-record(state, {tcp, pid_state, player_info, game_info}).

start_link(Pid_tcp, UID) ->
    gen_server:start_link({global, ?GLOBAL_PLAYER_PROCESS_NAME(UID)}, 
        ?MODULE, [Pid_tcp, UID], []).  

init([Pid_tcp, UID]) -> 
    Player_info = dm_database:get_player_info(UID),
    erlang:send_after(timer:seconds(30), self(), {'$gen_cast',check_tcp_alive}),
    {ok, #state{tcp=Pid_tcp, pid_state=working , player_info=Player_info}}.

handle_call({Module, Msg}, {From, _Ref}, State) ->
    process_protocol(Module, From, Msg, State, call);
handle_call(Msg, {From, _Ref}, State) ->
    process_protocol(undefined, From, Msg, State, call).

handle_cast({Module, From, Msg}, State) ->
    process_protocol(Module, From, Msg, State, cast);
handle_cast(Msg, State) ->
    process_protocol(undefined, undefined, Msg, State, cast).

handle_info(Msg, State) ->
    process_protocol(undefined, undefined, Msg, State, info).

terminate(_Reason, State) ->
    process_protocol(undefined, undefined, undefined, State, terminate).
    

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%==========================
%%Internal API
%%==========================

process_protocol(Module, From, Msg, State, Method) ->
    util:log_incoming_msg(?MODULE, Module, From, Msg, State, Method),
    case {Method, Module, Msg} of
        {terminate, _, _} ->
            graceful_termination(State);
        {info, _, timeout} ->
            {stop, normal, State};
        {cast, _, check_tcp_alive} ->
            process_check_tcp_alive(State);
        {cast, _, ?HEART_BEAT} ->
            process_heart_beat(State);
        {cast, _, {?GAME_PROTOCOL, Message}} ->
            process_game_message(State, Message);
        {cast, _, {?PROFILE_PROTOCOL, Message}} ->
            process_profile_message(State, Message);
        {cast, _, {?SOCIAL_PROTOCOL, Message}} ->
            process_social_message(State, Message);
        {cast, ?TCP_MODULE_NAME, ?RECONNECT_PLAYER} ->
            process_tcp_reconnect(State, From);
        {cast, ?TCP_MODULE_NAME, {?ENTER_ROOM, Game_type}} ->
            process_enter_room(State, Game_type);
        {cast, _, {?ENTER_ROOM_SUCCESS, Mod, Room_pid, Game_info}} ->
            process_enter_room_success(State, Mod, Room_pid, Game_info);
        {cast, ?TCP_MODULE_NAME, ?LOGOUT} ->
            process_logout(State, From);
        {cast, ?TCP_MODULE_NAME, ?TCP_CLOSED} ->
            process_tcp_closed_or_terminated(State, From);
        {cast, ?TCP_MODULE_NAME, ?TCP_TERMINATED} ->
            process_tcp_closed_or_terminated(State, From);
        _ ->
            process_unintended_msg(State, Method, Msg)
    end.

process_game_message(State, Message) ->
    case State#state.game_info of
        undefined ->
            {noreply, State};
        Game_info ->
            Mod  = Game_info#game_info.room_mod,
            {Player_info, Game_info2} = dm_game:on_player_message(Mod, State#state.tcp,
            State#state.player_info, State#state.game_info, Message),
            {noreply, State#state{player_info = Player_info, game_info = Game_info2}}
    end.

process_profile_message(State, _Message) ->
    {noreply, State}.

process_social_message(State, _Message) ->
    {noreply, State}.

graceful_termination(State) ->
    cast_pid(?PLAYER_PROCESS_TERMINATED, State#state.tcp),
    case State#state.game_info of
        undefined ->   
            ok;
        Game_info ->   
            cast_pid(?PLAYER_PROCESS_TERMINATED, Game_info#game_info.room_id)
    end.

process_enter_room(State, Game_type) ->
    case State#state.game_info of
        undefined ->
            gen_server:cast(dm_room_manager, {?ENTER_ROOM, Game_type, self()});
        Game_info ->
            Mod = Game_info#game_info.room_mod,
            to_client(State#state.tcp, ?MESSAGE_ENTER_ROOM_SUCCESS(
                Mod:info2client(State#state.player_info, Game_info)))
    end,
    {noreply, State}.

process_enter_room_success(State, Mod, Room_pid, Info) ->
    Game_info = #game_info{room_mod=Mod, room_id=Room_pid, info=Info},
    to_client(State#state.tcp, ?MESSAGE_ENTER_ROOM_SUCCESS(Mod:info2client(State#state.player_info, 
        Game_info))),
    {noreply, State#state{game_info = Game_info}}.

process_check_tcp_alive(State) ->
    case {State#state.tcp, State#state.pid_state} of
        {undefined, working} ->
            erlang:send_after(timer:seconds(30), self(), {'$gen_cast', check_tcp_alive}),
            {noreply, State#state{pid_state = tcp_lost}};
        {undefined, tcp_lost} ->
            erlang:send_after(timer:seconds(30), self(), {'$gen_cast', check_tcp_alive}),
            {noreply, State#state{pid_state = dying}};
        {undefined, dying} ->
            {stop, normal, State};
        {Tcp, _} ->
            case util:is_process_alive(Tcp) of
                true ->
                    erlang:send_after(timer:seconds(30), self(), {'$gen_cast', check_tcp_alive}),
                    {noreply, State#state{pid_state = working}};
                false ->
                    dm_log:error("dm_player process_check_tcp_alive defined tcp not alive . [~p]~n", [State]),
                    erlang:send_after(timer:seconds(30), self(), {'$gen_cast', check_tcp_alive}),
                    {noreply, State#state{tcp = undefined, pid_state = tcp_lost}}
            end
    end.

process_tcp_reconnect(State, From) ->
    case State#state.tcp of
        undefined ->
            ok;
        From ->
            ok;
        Else ->
            dm_log:backend("dm_player process_reconnect . old :~p , new :~p .~n", [State#state.tcp, From]),
            cast_pid(?RECONNECT_TO_OTHER_TCP, Else)
    end,
    Player_info = dm_database:get_player_info(State#state.player_info#player_info.uid),
    case {State#state.game_info, Player_info} of
        {_, undefined} ->
            cast_pid(?RECONNECT_ERROR_PLAYER, From),
            {noreply, State};
        {undefined, Player_info} ->
            cast_pid(?RECONNECT_IDLE_PLAYER, From),
            {noreply, State#state{tcp = From, pid_state = working, player_info = Player_info}};
        {Game_info, Player_info} ->
            Mod = Game_info#game_info.room_mod,
            cast_pid({?RECONNECT_BUSY_PLAYER, Mod:info2client(Player_info, Game_info)}, From),
            {noreply, State#state{tcp = From, pid_state = working, player_info = Player_info}}
    end.

process_heart_beat(State) ->
    dm_log:debug("dm_player process_heart_beat.~n"),
    case State#state.pid_state of
        dying ->
            {noreply, State#state{pid_state = tcp_lost}};
        _ ->
            {noreply, State}
    end.

process_logout(State, From) ->
    case State#state.tcp of
        From ->
            {stop, normal, State#state{tcp = undefined}};
        _ ->
            {noreply, State}
    end.

process_tcp_closed_or_terminated(State, From) ->
    case State#state.tcp of
        From ->
            {noreply, State#state{tcp = undefined, pid_state = tcp_lost}};
        _ ->
            {noreply, State}
    end.

process_unintended_msg(State, Method, Msg) ->
    case Method of
        cast ->
            dm_log:error("unknown cast message to dm_player : ~p, state: ~p. ~n", [Msg, State]),
            {noreply, State};
        call ->
            dm_log:error("unknown call message to dm_player : ~p, state: ~p. ~n", [Msg, State]),
            {reply, {ok, Msg}, State};
        info ->
            dm_log:error("unknown info message to dm_player : ~p, state: ~p. ~n", [Msg, State]),
            {noreply, State}
    end.

to_client(Tcp, Message) ->
    cast_pid({?SEND_MSG_TO_CLIENT, Message}, Tcp).

to_room(Room, Message) ->
    cast_pid({?GAME_PROTOCOL, Message}, Room).

to_peer(Player, Message) ->
    cast_pid({?GAME_PROTOCOL, Message}, Player).
    
cast_pid(Msg, Pid) ->
    case Pid of 
        undefined -> ok;
        _ -> gen_server:cast(Pid, {?MODULE, self(), Msg})
    end.
