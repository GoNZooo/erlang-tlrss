-module(tlrss_looper_supervisor).

-behaviour(supervisor).

-export([start_link/0,
         start_child/2,
         terminate_child/1,
         init/1]).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_child(string(), non_neg_integer()) -> supervisor:startchild_ret().
start_child(Feed, Sleeptime) ->
    supervisor:start_child(?MODULE, [Feed, Sleeptime]).

-type terminate_error() :: not_found | simple_one_for_one.
-spec terminate_child(pid()) -> ok | {error, terminate_error()}.
terminate_child(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 10,
                 period => 60000},
    Children = [#{id => tlrss_looper,
                  start => {tlrss_looper, start_link, []},
                  restart => transient}],

    {ok, {SupFlags, Children}}.
