-module(tp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(DEFAULT_PORT, 1235).

-define(TCP_OPTIONS, [binary, {active, true}, {packet, 0}, {reuseaddr, true}]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	init(),
	Port = case application:get_env(tcp_interface, port) of
               {ok, P} -> P;
               undefined -> ?DEFAULT_PORT
           end,
    {ok, LSock} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    tp_sup:start_link(LSock).
            
stop(_State) ->
    ok.

%% ===================================================================
%% Internal callbacks
%% ===================================================================

init() ->
	tp_database:init(),
	ok.