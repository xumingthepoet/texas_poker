-module(dm_player).

-include("dm_protocol.hrl").

-behaviour(gen_server).

-export([start_link/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {tcp, pid_state, player_info, game_info}).

start_link(Pid_tcp, UID) ->
    gen_server:start_link({global, ?GLOBAL_PLAYER_PROCESS_NAME(UID)}, ?MODULE, [Pid_tcp, UID], []).  

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
    case {Method, Module, Msg} of
        {terminate, _, _} ->
            graceful_termination(State);
        {info, _, timeout} ->
            {stop, normal, State};
        {call, ?TCP_MODULE_NAME, ?RECONNECT_PLAYER} ->
            process_reconnect(State, From);
        {call, ?TCP_MODULE_NAME, {?ENTER_ROOM, Game_type}} ->
            process_enter_room(State, Game_type);
        {cast, _, check_tcp_alive} ->
            process_check_tcp_alive(State);
        {cast, ?TCP_MODULE_NAME, ?LOGOUT} ->
            process_logout(State, From);
        {cast, ?TCP_MODULE_NAME, ?HEART_BEAT} ->
            process_heart_beat(State);
        {cast, ?TCP_MODULE_NAME, ?TCP_CLOSED} ->
            process_tcp_closed_or_terminated(State, From);
        {cast, ?TCP_MODULE_NAME, ?TCP_TERMINATED} ->
            process_tcp_closed_or_terminated(State, From);
        _ ->
            process_unintended_msg(State, Method, Msg)
    end.

graceful_termination(State) ->
    cast_pid(?PLAYER_PROCESS_TERMINATED, State#state.tcp),
    case State#state.game_info of
        undefined ->   
            ok;
        Game_info ->   
            cast_pid(?PLAYER_PROCESS_TERMINATED, Game_info#game_info.room_id)
    end.

process_enter_room(State, Game_type) ->
    {ok, Game_info} = gen_server:call(dm_room_manager, {?ENTER_ROOM, Game_type}),
    State#state{game_info = Game_info}.
    
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

process_reconnect(State, From) ->
    case State#state.tcp of
        From ->
            ok;
        Else ->
            dm_log:backend("dm_player process_reconnect . old :~p , new :~p .", [State#state.tcp, From]),
            cast_pid(?RECONNECT_TO_OTHER_TCP, Else)
    end,
    Player_info = dm_database:get_player_info(State#state.player_info#player_info.uid),
    case {State#state.game_info, Player_info} of
        {_, undefined} ->
            {reply, {error, get_player_info_failed}, State};
        {undefined, Player_info} ->
            {reply, {ok, ?IDLE_PLAYER}, State#state{tcp = From, pid_state = working, player_info = Player_info}};
        {Game_info, Player_info} ->
            {ok , Game} = call_pid(?RECONNECT_PLAYER, Game_info#game_info.room_id),
            {reply, {ok, ?BUSY_PLAYER, Game}, State#state{tcp = From, pid_state = working, player_info = Player_info}}
    end.

process_heart_beat(State) ->
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

send_msg_to_client(State, Message) ->
    cast_pid({?SEND_MSG_TO_CLIENT, Message}, State#state.tcp).
    
call_pid(Msg, Pid) ->
    case Pid of
        undefined -> ok;
        _ -> gen_server:call(Pid, {?MODULE, Msg})
    end.

cast_pid(Msg, Pid) ->
    case Pid of 
        undefined -> ok;
        _ -> gen_server:cast(Pid, {?MODULE, self(), Msg})
    end.
