-module(tlrss_download_supervisor).

-export([start_link/0,
         init/1,
         start_child/2,
         terminate_child/1]).

-behaviour(supervisor).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-type download_type() :: feed | torrent.

-spec start_child(download_type(), string()) -> supervisor:startchild_ret().
start_child(feed, Url) ->
    supervisor:start_child(?MODULE, [feed, Url]);
start_child(torrent, Url) ->
    supervisor:start_child(?MODULE, [torrent, Url]). 

-spec terminate_child(pid()) -> ok.
terminate_child(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 10,
                 period => 3600000},
    Children = [#{id => tlrss_downloader,
                  start => {tlrss_downloader, start_download, []},
                  restart => transient}],

    {ok, {SupFlags, Children}}.
