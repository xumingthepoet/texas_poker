-module(dm_room_manager).

-include("dm_protocol.hrl").

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {current_room}).

start_link() ->
	dm_log:debug("dm_room_manager start_link\n"), 
    gen_server:start_link(?MODULE, [], []).

init([]) ->
	register(?MODULE, self()),
    {ok, #state{}}.

handle_call({?ENTER_ROOM, Game_type}, {From, _Ref}, State) ->
    Game_info = handle_enter_room(Game_type, From, State),
    {reply, {ok, Game_info}, State}.
handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

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
    
    ok.