-module(dm_player_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/2]).

-export([init/1]).

start_link() ->
	dm_log:debug("dm_player_sup start_link\n"), 
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Pid_tcp, UID) ->
	dm_log:debug("dm_player_sup start_child\n"),
    supervisor:start_child(?MODULE, [Pid_tcp, UID]).

init([]) ->
    Children = [{dm_player, {dm_player, start_link, []},
                temporary, brutal_kill, worker, [dm_player]}],
    {ok, {{simple_one_for_one, 0, 1}, Children}}.