-module(tlrss_looper).

-export([]).

-include("records.hrl").

%-compile([{parse_transform, lager_transform}]).

-export([start_link/2,
         ensure_slash/1]).

-spec start_link(string(), non_neg_integer()) -> {ok, pid()}.
start_link(Feed, Sleeptime) ->
    {ok, spawn_link(fun() -> loop(Feed, Sleeptime) end)}.

loop(Feed, Sleeptime) ->
    {ok, Dir} = application:get_env(tlrss, download_dir),
    DownloadDir = ensure_slash(Dir),

    Items = tlrss_downloader:download_wait(feed, Feed),
    {new_items, NewItems} = tlrss_item_bucket:add(Items),
    {filtered_items, FilteredItems} = tlrss_item_filter:filter(NewItems),

    TorrentData = get_torrent_data(FilteredItems), 
    write_torrents(DownloadDir, TorrentData),

    timer:sleep(Sleeptime),
    loop(Feed, Sleeptime).

get_torrent_data(Items) ->
    get_torrent_data(Items, []).

get_torrent_data([], Output) ->
    Output;
get_torrent_data([I | Is], Output) ->
    Url = binary:bin_to_list(I#item.download_link),
    UrlComponents = re:split(Url, "/"),
    FilenameBinary = lists:last(UrlComponents),
    FilenameString = binary:bin_to_list(FilenameBinary),

    Data = tlrss_downloader:download_wait(torrent, Url),
    get_torrent_data(Is, [{FilenameString, Data} | Output]).

write_torrents(_, []) ->
    ok;
write_torrents(DownloadDir, [{Filename, Data} | Ts]) ->
    DownloadPath = DownloadDir ++ Filename,
    file:write_file(DownloadPath, Data, [write, binary]),
    lager:info("Downloading: ~p~n", [Filename]),
    write_torrents(DownloadDir, Ts).


ensure_slash(Dir) ->
    case lists:last(Dir) of
        $/ -> Dir;
        _ -> Dir ++ "/"
    end.
