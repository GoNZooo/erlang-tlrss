-module(tlrss_looper).

-compile({parse_transform, do}).

-export([]).

-include("records.hrl").

-export([start_link/2,
         ensure_slash/1]).

-spec start_link(Feed :: string(), Sleeptime :: non_neg_integer()) -> {ok, pid()};
                ({Feed :: string(), Filters :: [binary()], no_global},
                 Sleeptime :: non_neg_integer()) -> {ok, pid()}.
start_link({Feed, Filters, no_global}, Sleeptime) ->
    Compiled = lists:map(fun({F, Opts}) ->
                                 {ok, Regex} = re:compile(F, Opts),
                                 Regex
                         end, Filters),
    {ok, spawn_link(fun() -> loop({Feed, Compiled, no_global}, Sleeptime) end)};
start_link(Feed, Sleeptime) ->
    {ok, spawn_link(fun() -> loop(Feed, Sleeptime) end)}.

loop({Feed, Filters, no_global} = FeedSpec, Sleeptime) ->
    {ok, Dir} = application:get_env(tlrss, download_dir),
    DownloadDir = ensure_slash(Dir),

    case tlrss_downloader:download(feed, Feed) of
        {ok, Items} ->
            {new_items, NewItems} = tlrss_item_bucket:add(Items),
            {filtered_items, FilteredItems} = tlrss_item_filter:filter(NewItems, Filters),

            TorrentData = get_torrent_data(FilteredItems),
            write_torrents(DownloadDir, TorrentData);
        {error, Reason} ->
            lager:error("~p~n", [Reason])
    end,

    timer:sleep(Sleeptime),
    loop(FeedSpec, Sleeptime);
loop(Feed, Sleeptime) ->
    {ok, Dir} = application:get_env(tlrss, download_dir),
    DownloadDir = ensure_slash(Dir),

    case tlrss_downloader:download(feed, Feed) of
        {ok, Items} ->
            {new_items, NewItems} = tlrss_item_bucket:add(Items),
            {filtered_items, FilteredItems} = tlrss_item_filter:filter(NewItems),

            TorrentData = get_torrent_data(FilteredItems),
            write_torrents(DownloadDir, TorrentData);
        {error, Reason} ->
            lager:error("~p~n", [Reason])
    end,

    timer:sleep(Sleeptime),
    loop(Feed, Sleeptime).

-type torrent_data() :: {string(), binary()}.
-spec get_torrent_data([#item{}]) -> [torrent_data()].
get_torrent_data(Items) ->
    get_torrent_data(Items, []).

-spec get_torrent_data([#item{}], [torrent_data()]) -> [torrent_data()].
get_torrent_data([], Output) ->
    Output;
get_torrent_data([I | Is], Output) ->
    Url = binary:bin_to_list(I#item.download_link),
    UrlComponents = re:split(Url, "/"),
    FilenameBinary = lists:last(UrlComponents),
    FilenameString = binary:bin_to_list(FilenameBinary),

    Data = tlrss_downloader:download(torrent, Url),
    get_torrent_data(Is, [{FilenameString, Data} | Output]).

-spec write_torrents(string(), [{string(), binary()}]) -> ok.
write_torrents(_, []) ->
    ok;
write_torrents(DownloadDir, [{Filename, Data} | Ts]) ->
    DownloadPath = DownloadDir ++ Filename,
    do([error_m ||
           file:write_file(DownloadPath, Data, [write, binary]),
           lager:info("Downloading: ~p~n", [Filename]),
           write_torrents(DownloadDir, Ts)
       ]).

-spec ensure_slash(string()) -> string().
ensure_slash(Dir) ->
    case lists:last(Dir) of
        $/ -> Dir;
        _ -> Dir ++ "/"
    end.
