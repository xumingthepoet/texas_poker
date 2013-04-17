-module(dm_tcp).

-include("dm_protocol.hrl").

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {lsock, client, player}).

start_link(Lsock) ->
    gen_server:start_link(?MODULE, [Lsock], []).

init([Lsock]) ->
    erlang:send_after(timer:seconds(10), self(), {'$gen_cast',check_player_alive}),
    {ok, #state{lsock = Lsock}, 0}.

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
%% Internal API
%%==========================

process_protocol(Module, From, Msg, State, Method) ->
    case {Method, Module, Msg} of
        {terminate, _, _} ->
            graceful_termination(State);
        {info, _, {tcp, _Socket, Request}} ->
            {noreply, process_tcp_request(Request, State)};
        {info, _, {tcp_closed, _Socket}} ->
            when_tcp_close(State);
        {info, _, timeout} ->
            start_a_new_tcp(State);
        {cast, _, check_player_alive} ->
            process_check_player_alive(State);
        {cast, ?PLAYER_MODULE_NAME, {?SEND_MSG_TO_CLIENT, Message}} ->
            process_send_msg_to_client(State, Message);
        {cast, ?PLAYER_MODULE_NAME, ?PLAYER_PROCESS_TERMINATED} ->
            process_player_process_termination(State, From, Msg);
        {cast, ?PLAYER_MODULE_NAME, ?RECONNECT_TO_OTHER_TCP} ->
            process_reconnect_to_other_tcp(State);
        _ ->
            process_unintended_msg(State, Method, Msg)
    end.

graceful_termination(State) ->
    dm_log:debug("dm_tcp terminated, State : ~p.~n", [State]),
    cast_player(?TCP_TERMINATED, State#state.player),
    (catch gen_tcp:close(State#state.client)).

when_tcp_close(State) ->
    dm_log:backend("tcp closed , State: ~p .~n", [State]),
    cast_player(?TCP_CLOSED, State#state.player),
    {stop, normal, State}.

start_a_new_tcp(State) ->
    Lsock = State#state.lsock,
    {ok, Rsock} = gen_tcp:accept(Lsock),
    dm_tcp_sup:start_child(),
    cast_client(?MESSAGE_TCP_CONNECTED, Rsock),
    {noreply, #state{lsock = Lsock ,client = Rsock}}.

process_send_msg_to_client(State, Message) ->
    cast_client(Message, State#state.client),
    {noreply, State}.

process_check_player_alive(State) ->
    case State#state.player of
        undefined ->
            erlang:send_after(timer:seconds(10), self(), {'$gen_cast', check_player_alive}),
            {noreply, State};
        P ->
            case util:is_process_alive(P) of
                true ->
                    erlang:send_after(timer:seconds(10), self(), {'$gen_cast', check_player_alive}),
                    {noreply, State};
                false ->
                    {stop, normal, State#state{player = undefined}}
            end
    end.

process_player_process_termination(State, From, Msg) ->
    case State#state.player of 
        From ->
            {stop, normal, State};
        _ ->
            dm_log:error("\"PLAYER_PROCESS_TERMINATED\" message from unknown player process : ~p, state: ~p. ~n", [Msg, State]),
            {noreply, State}
    end.

process_reconnect_to_other_tcp(State) ->
    dm_log:backend("tcp terminated for player reconnecting_to_other_tcp: ~p .~n", [State]),
    {stop, normal, State#state{player = undefined}}.
    
process_tcp_request(<<>>, State) ->
    State;
process_tcp_request(Request, State) ->
    try 
        <<Length:4/big-signed-integer-unit:8, Json:Length/binary ,Rest/binary>> = Request,
        NewState = process_single_request(Json, State),
        process_tcp_request(Rest, NewState)
    catch
        %Class:Err ->
        %    dm_log:error("process client request: ~p failed. {~p, ~p}~n", [Request, Class, Err]),
        %    State
        _ -> %% let it crash!
            State
    end.

process_single_request(Json, State) ->
    dm_log:backend("client request: ~p .~n", [Json]),
    {Msg} = dm_protocol:json2msg(Json),
    Api = proplists:get_value(?API, Msg),
    case Api of
        ?LOGIN -> 
            login(proplists:get_value(?UID, Msg), State);
        ?LOGOUT -> 
            logout(State);
        ?ENTER_ROOM ->
            enter_room(proplists:get_value(?GAME_TYPE, Msg), State);
        ?GAME_PROTOCOL ->
            game_protocol(proplists:get_value(?GAME_ACTION, Msg), State);
        _Other ->   
            cast_player(Msg, State#state.player),
            State
    end.

login(UID, State) ->
    dm_log:debug("dm_tcp login, uid = ~p .~n", [UID]),
    gen_server:cast(global:whereis_name(?GLOBAL_PLAYER_PROCESS_NAME(UID)), ?HEART_BEAT),
    timer:sleep(100),
    Pid_player2 =   global:whereis_name(?GLOBAL_PLAYER_PROCESS_NAME(UID)),
    Pid_player  =   case Pid_player2 of
                        undefined ->
                            try
                                {ok, Pid} = dm_player_sup:start_child(self(), UID),
                                cast_client(?MESSAGE_LOGIN_SUCCESS, State#state.client),
                                Pid
                            catch
                                _:_ ->
                                    dm_log:error("dm_tcp: dm_player_sup:start_child failed. UID:~p.~n", [UID]),
                                    cast_client(?MESSAGE_LOGIN_FAILED, State#state.client),
                                    undefined
                            end;
                        _ ->
                            try            
                                case call_player(?RECONNECT_PLAYER, Pid_player2) of
                                    {ok, ?IDLE_PLAYER}  ->
                                        cast_client(?MESSAGE_RECONNECT_SUCCESS, State#state.client),
                                        Pid_player2;
                                    {ok, ?BUSY_PLAYER, Msg} ->
                                        cast_client(?MESSAGE_RECONNECT_SUCCESS_2(Msg), State#state.client),
                                        Pid_player2;
                                    _ ->
                                        dm_log:error("dm_tcp: reconnect_player failed, unknown protocol. UID:~p.~n", [UID]),
                                        cast_client(?MESSAGE_RECONNECT_FAILED, State#state.client),
                                        undefined
                                end
                            catch
                                _:_ ->
                                    dm_log:error("dm_tcp: reconnect_player call_player failed. UID:~p.~n", [UID]),
                                    cast_client(?MESSAGE_RECONNECT_FAILED, State#state.client),
                                    undefined
                            end
                    end,
    State#state{player = Pid_player}.

logout(State) ->
    cast_player(?LOGOUT, State#state.player),
    State#state{player=undefined}.

enter_room(GameType, State) ->
    call_player({?ENTER_ROOM, GameType}, State#state.player),
    State.

game_protocol(Action, State) ->
    cast_player({?GAME_ACTION, Action}, State#state.player),
    State.

process_unintended_msg(State, Method, Msg) ->
    case Method of
        cast ->
            dm_log:error("unknown cast message to dm_tcp : ~p, state: ~p. ~n", [Msg, State]),
            {noreply, State};
        call ->
            dm_log:error("unknown call message to dm_tcp : ~p, state: ~p. ~n", [Msg, State]),
            {reply, {ok, Msg}, State};
        info ->
            dm_log:error("unknown info message to dm_tcp : ~p, state: ~p. ~n", [Msg, State]),
            {noreply, State}
    end.

cast_player(Msg, Player) ->
    case Player of
        undefined -> ok;
        Player2 ->  gen_server:cast(Player2, {?MODULE, self(), Msg})
    end.

call_player(Msg, Player) ->
    case Player of 
        undefined -> {error, undefined_player};
        Player2 -> 
            Time1 = erlang:now(),
            Result = gen_server:call(Player2, {?MODULE, Msg}),
            Time_diff = util:time_diff(Time1),
            dm_log:backend("Request : ~p , time using = ~p us.", [Msg, Time_diff]),
            Result
    end.

cast_client(Msg, Client) ->
    case Client of
        undefined -> ok;
        Client2 -> dm_protocol:send(Client2, Msg)
    end.




