-module(tlrss_downloader).

-include("records.hrl").

-export([start_download/2,
         download/2]).

download(feed, Url) ->
    {ok, Pid} = tlrss_download_supervisor:start_child(feed, Url),
    get_data(Pid);
download(torrent, Url) ->
    {ok, Pid} = tlrss_download_supervisor:start_child(torrent, Url),
    get_data(Pid).

start_download(feed, Url) ->
    {ok, spawn_link(fun() -> Data = download_feed(Url),
                             receive
                                 {From, get_data} ->
                                     From ! {ok, Data}
                             after 5000 ->
                                     {error, not_claimed}
                             end
                    end)};
start_download(torrent, Url) ->
    {ok, spawn_link(fun() -> Data = download_torrent(Url),
                             receive
                                 {From, get_data} ->
                                     From ! {ok, Data}
                             after 5000 ->
                                     {error, not_claimed}
                             end
                    end)}.

get_data(Pid) ->
    Pid ! {self(), get_data},
    receive
        {ok, Data} ->
            Data

    after 5000 ->
            {error, not_claimed}
    end.

fetch_data(Url) ->
    fetch_data(Url, string).

fetch_data(Url, string) ->
    {ok, {_, _, Data}} = httpc:request(Url),
    Data;
fetch_data(Url, binary) ->
    {ok, {_, _, Data}} = httpc:request(get,
                                       {Url, []},
                                       [],
                                       [{body_format, binary}]),
    Data.

download_feed(Url) ->
    Data = fetch_data(Url),
    {ok, {_, Entries}, _Rest} = feeder:stream(Data, []),
    lists:map(fun entry_to_item/1, Entries).

download_torrent(Url) ->
    fetch_data(Url, binary).

entry_to_item({entry, undefined, undefined, undefined,
               ID, undefined, Download, undefined, Category,
               Name, DateUploaded}) ->
    #item{id = ID,
          download_link = Download,
          name = Name,
          date_uploaded = DateUploaded,
          category = Category}.
