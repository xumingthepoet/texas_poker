-module(dm_tcp_sup).

-behaviour(supervisor).

-export([start_link/1, start_child/0]).

-export([init/1]).

start_link(Lsock) ->
	dm_log:debug("dm_tcp_sup start_link\n"),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Lsock]).

start_child() ->
	dm_log:debug("dm_tcp_sup start_child\n"),
    supervisor:start_child(?MODULE, []).

init([Lsock]) ->
    {ok, 
        {{simple_one_for_one, 0, 1}, 
    	   	[{dm_tcp, {dm_tcp, start_link, [Lsock]}, 
    			temporary, brutal_kill, worker, [dm_tcp]}]}}.