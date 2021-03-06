-module(dm_room).

-behaviour(gen_server).

-include("dm_protocol.hrl").

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([to_self/2, to_client/2, to_player/2, broadcast/2, send_after/2]).

-record(state, {game_mod, room_info}).

start_link(Mod) ->
    dm_log:debug("dm_room start_link\n"),
    gen_server:start_link(?MODULE, [Mod], []).

init([Mod]) ->
    %erlang:send_after(timer:seconds(1), self(), {'$gen_cast',timer}),
    <<A1:32,A2:32,A3:32>> = crypto:rand_bytes(12),
    random:seed(A1,A2,A3),
    {ok, #state{game_mod = Mod, room_info = Mod:init_room()}}.

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

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%==========================
%% Internal API
%%==========================

process_protocol(Module, From, Msg, State, Method) ->
    util:log_incoming_msg(?MODULE, Module, From, Msg, State, Method),
    case {Method, Module, Msg} of
        {info, _, timeout} ->
            {stop, normal, State};
        {cast, _, {?ENTER_ROOM_REQUEST, Player}} ->
            Mod = State#state.game_mod,
            {NewStateInfo, Player_game_info} = Mod:enter_room_request(State#state.room_info, Player),
            gen_server:cast(Player, {?ENTER_ROOM_SUCCESS, State#state.game_mod, self(), Player_game_info}),
            {noreply, State#state{room_info=NewStateInfo}};
        {cast, _, {?PLAYER_PROCESS_TERMINATED, Player_game_info}} ->
            Mod = State#state.game_mod,
            Room_info = Mod:leave_room(State#state.room_info, Player_game_info ,From),
            {noreply, State#state{room_info=Room_info}};
        {cast, _, {?GAME_PROTOCOL, Action}} ->
            process_game_message(State, Action, From);
        %{cast, _, timer} ->
        %    process_game_timer(State);
        _ ->
            process_unintended_msg(State, Method, Msg)
    end.

%deprate
%process_game_timer(State) ->
%    erlang:send_after(timer:seconds(1), self(), {'$gen_cast',timer}),
%    #state{game_mod=Mod, room_info=Game_info} = State,
%    Game_info2 = Mod:on_room_timer(Game_info),
%    {noreply, State#state{room_info = Game_info2}}.

process_game_message(State, Message, From) ->
    #state{game_mod=Game_Mod, room_info=Game_info} = State,
    Game_info2 = Game_Mod:on_room_message(Game_info, Message, From),
    {noreply, State#state{room_info = Game_info2}}.

process_unintended_msg(State, Method, Msg) ->
    case Method of
        cast ->
            dm_log:error("unknown cast message to dm_room : ~p, state: ~p. ~n", [Msg, State]),
            {noreply, State};
        call ->
            dm_log:error("unknown call message to dm_room : ~p, state: ~p. ~n", [Msg, State]),
            {reply, {ok, Msg}, State};
        info ->
            dm_log:error("unknown info message to dm_room : ~p, state: ~p. ~n", [Msg, State]),
            {noreply, State}
    end.

send_after(Seconds, Msg) ->
    erlang:send_after(timer:seconds(Seconds), self(), {'$gen_cast',{?MODULE, self(), {?GAME_PROTOCOL, Msg}}}).

broadcast([], _Msg) ->
    ok;
broadcast([H|T], Msg) ->
    to_player(H, Msg),
    broadcast(T, Msg).

to_client(Player, Message) ->
    cast_pid({?SEND_MSG_TO_CLIENT, Message}, Player).

to_player(Player, Message) ->
    cast_pid({?GAME_PROTOCOL, Message}, Player).

to_self(Room, Message) ->
    cast_pid({?GAME_PROTOCOL, Message}, Room).

cast_pid(Msg, Pid) ->
    case Pid of 
        undefined -> ok;
        _ -> gen_server:cast(Pid, {?MODULE, self(), Msg})
    end.

