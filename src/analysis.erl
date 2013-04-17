-module(analysis).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

start_link() ->
	dm_log:debug("analysis start_link\n"),
    gen_server:start_link(?MODULE, [], []). 

init([]) ->
	register(?MODULE, self()),
    {ok, #state{}}. 

handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast({?MODULE, stop}, State) ->
    {stop, normal, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal functions

