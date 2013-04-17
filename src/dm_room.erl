-module(dm_room).

-behaviour(gen_server).

-include("dm_protocol.hrl").

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).

start_link() ->
    dm_log:debug("dm_room start_link\n"),
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    <<A1:32,A2:32,A3:32>> = crypto:rand_bytes(12),
    random:seed(A1,A2,A3),
    erlang:send_after(timer:seconds(1), self(), {'$gen_cast',timer}),
    {ok, #state{}}.

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
    case {Method, Module, Msg} of
        {info, _, timeout} ->
            {stop, normal, State};
        {cast, _, timer} ->
            erlang:send_after(timer:seconds(1), self(), {'$gen_cast',timer}),
            {noreply, State};
        _ ->
            process_unintended_msg(State, Method, Msg)
    end.

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