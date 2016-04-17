-module(tlrss_looper_supervisor).

-behaviour(supervisor).

-export([start_link/0,
         start_child/2,
         terminate_child/1,
         init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Feed, Sleeptime) ->
    supervisor:start_child(?MODULE, [Feed, Sleeptime]).

terminate_child(Pid) ->
    supervisor:terminate_child(Pid).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 10,
                 period => 60000},
    Children = [#{id => tlrss_looper,
                  start => {tlrss_looper, start_link, []},
                  restart => transient}],

    {ok, {SupFlags, Children}}.
