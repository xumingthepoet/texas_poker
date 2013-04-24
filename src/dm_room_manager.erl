-module(dm_room_manager).

-include("dm_protocol.hrl").
-include("custom_protocol.hrl").

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,{current}).

start_link() ->
	dm_log:debug("dm_room_manager start_link\n"), 
    gen_server:start_link(?MODULE, [], []).

init([]) ->
	register(?MODULE, self()),
    {ok, #state{}}.
    %{ok, ets:new(?MODULE, [])}.

handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast({?ENTER_ROOM, Game_type, From}, State) ->
    handle_enter_room(Game_type, From, State);
handle_cast({?MODULE, stop}, State) ->
    {stop, normal, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%=======================================
%% Internal API
%%=======================================

handle_enter_room(Game_type, From, State) ->
    case State#state.current of
        undefined ->
            Mod = ?GAME_TYPE_TO_MOD(Game_type),
            case Mod of 
                undefined ->
                    {noreply, State};
                _ ->
                    {ok, Room_pid} = dm_room_sup:start_child(Mod),
                    gen_server:cast(Room_pid, {?ENTER_ROOM_REQUEST, From}),
                    {noreply, State#state{current = Room_pid}}
            end;
        Room ->
            Room_pid = Room,
            gen_server:cast(Room_pid, {?ENTER_ROOM_REQUEST, From}),
            {noreply, State#state{current = Room_pid}}
    end.
    

