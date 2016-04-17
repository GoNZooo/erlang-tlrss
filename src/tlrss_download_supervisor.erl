-module(tlrss_download_supervisor).

-export([start_link/0,
         init/1,
         start_child/2]).

-behaviour(supervisor).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(feed, Url) ->
    supervisor:start_child(?MODULE, [feed, Url]);
start_child(torrent, Url) ->
    supervisor:start_child(?MODULE, [torrent, Url]). 

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 10,
                 period => 3600000},
    Children = [#{id => tlrss_downloader,
                  start => {tlrss_downloader, start_link, []},
                  restart => transient}],

    {ok, {SupFlags, Children}}.
