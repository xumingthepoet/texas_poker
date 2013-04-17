-module(dm_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, start_child/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Lsock) ->
    dm_log:debug("dm_sup start_link\n"),
	supervisor:start_link({local, ?MODULE}, ?MODULE, [Lsock]).

start_child() ->
    dm_log:debug("dm_sup start_child\n"),
    supervisor:start_child(?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Lsock]) ->
	Tcp_Sup = {dm_tcp_sup, {dm_tcp_sup, start_link, [Lsock]},
                permanent, 2000, supervisor, [dm_tcp_sup]},  
	Player_Sup = {dm_player_sup, {dm_player_sup, start_link, []},
              	    permanent, 2000, supervisor, [dm_player_sup]},
    Room_Sup = {dm_room_sup, {dm_room_sup, start_link, []},
                permanent, 2000, supervisor, [dm_room_sup]},
    Room_Manager = {dm_room_manager, {dm_room_manager, start_link, []},
                	permanent, 2000, worker, [dm_room_manager]},
    Analysis = {analysis, {analysis, start_link, []},
                permanent, 2000, worker, [analysis]},
    Children = [Tcp_Sup, Player_Sup, Room_Sup, Room_Manager, Analysis],  
    {ok, {{one_for_one, 5, 3600}, Children}}.


