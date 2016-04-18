-module(tlrss_looper_manager).

-include("records.hrl").

-export([feeds/0,
         add/1]).

-export([start_link/2,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-behaviour(gen_server).

-spec start_link([string()], non_neg_integer()) -> gen_server:startlink_ret().
start_link(Feeds, Sleeptime) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Feeds, Sleeptime], []).

-spec feeds() -> [string()].
feeds() ->
    gen_server:call(?MODULE, feeds).

-spec add(string()) -> ok.
add(Feed) ->
    gen_server:call(?MODULE, {add, Feed}).

init([Feeds, Sleeptime]) ->
    FeedMap = start_feeds(Feeds, Sleeptime),

    {ok, FeedMap}.

-spec start_feeds([string()], non_neg_integer()) -> #{}.
start_feeds(Feeds, Sleeptime) ->
    start_feeds(Feeds, Sleeptime, []).

-spec start_feeds([string()], non_neg_integer(), [{string(), pid()}]) -> #{}.
start_feeds([], Sleeptime, Output) ->
    StateList = [{sleeptime, Sleeptime} | Output],
    maps:from_list(StateList);
start_feeds([F | Fs], Sleeptime, Output) ->
    {ok, Pid} = tlrss_looper_supervisor:start_child(F, Sleeptime),
    start_feeds(Fs, Sleeptime, [{F, Pid} | Output]).

handle_call(feeds, _From, Feeds) ->
    {reply, Feeds, Feeds};
handle_call({remove, Feed}, _From, Feeds) ->
    case Feeds of
        #{Feed := Pid} ->
            tlrss_looper_supervisor:terminate_child(Pid),

            {reply, {ok, {removed, Feed}}, maps:remove(Feed, Feeds)};
        _ ->
            {error, incorrect_feed}
    end.

handle_cast({add, Feed}, #{sleeptime := Sleeptime} = Feeds) ->
    {ok, Pid} = tlrss_looper_supervisor:start_child(Feed, Sleeptime),
    NewFeeds = Feeds#{Feed => Pid},

    {noreply, NewFeeds};
handle_cast(_Msg, N) ->
    {noreply, N}.

handle_info(_Info, N) ->
    {noreply, N}.

terminate(_Reason, _N) ->
    ok.

code_change(_OldVsn, N, _Extra) ->
    {ok, N}.
