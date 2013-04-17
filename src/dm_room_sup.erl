-module(dm_room_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/0]).

-export([init/1]).

start_link() ->
	dm_log:debug("dm_room_sup start_link\n"), 
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child() ->
	dm_log:debug("dm_room_sup start_child\n"), 
    supervisor:start_child(?MODULE, []).

init([]) ->
    Room = {dm_room, {dm_room, start_link, []}, temporary, 
    		brutal_kill, worker, [dm_room]},
    Children = [Room],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.