-module(tlrss_downloader).

-include("records.hrl").

-export([start_download/2,
         download/2,
         fetch_data/1]).

-spec download(feed, string()) -> string();
              (torrent, string()) -> binary().
download(feed, Url) ->
    {ok, Pid} = tlrss_download_supervisor:start_child(feed, Url),
    get_data(Pid);
download(torrent, Url) ->
    {ok, Pid} = tlrss_download_supervisor:start_child(torrent, Url),
    get_data(Pid).

-spec start_download(feed, string()) -> {ok, pid()};
                    (torrent, string()) -> {ok, pid()}.
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

-spec get_data(pid()) -> binary() | string() | {error, not_claimed}.
get_data(Pid) ->
    Pid ! {self(), get_data},
    receive
        {ok, Data} ->
            Data

    after 30000 ->
            {error, not_claimed}
    end.

-spec fetch_data(string()) -> string().
fetch_data(Url) ->
    fetch_data(Url, string).

-spec fetch_data(string(), string) -> string();
                (string(), binary) -> binary().
fetch_data(Url, string) ->
    case httpc:request(Url) of
        {ok, {_, _, Data}} -> {ok, Data};
        {error, Reason} -> {error, Reason}
    end;
fetch_data(Url, binary) ->
    case httpc:request(get,
                       {Url, []},
                       [],
                       [{body_format, binary}]) of
        {ok, {_, _, Data}} -> {ok, Data};
        {error, Reason} -> {error, Reason}
    end.

-spec try_parse_rss(Data :: string()) -> Result :: {ok, [#item{}]}
                                                 | {error, term()}.
try_parse_rss(Data) ->
    try feeder:stream(Data, []) of
        {ok, {_, Entries}, _Rest} ->
            {ok, lists:map(fun entry_to_item/1, Entries)};
        {fatal_error, _, _, _, _} ->
            {error, "Invalid RSS data / Timeout"}
    catch
        error:function_clause ->
            {error, "Invalid RSS data / Timeout"}
    end.

-spec download_feed(string()) -> [#item{}].
download_feed(Url) ->
    case fetch_data(Url) of
        {ok, Data} -> 
            try_parse_rss(Data);
        {error, Reason} ->
            {error, Reason}
    end.

-spec download_torrent(string()) -> binary().
download_torrent(Url) ->
    {ok, Data} = fetch_data(Url, binary),
    Data.

-type rss_entry() :: {entry, undefined, undefined, undefined,
                      binary(), undefined, binary(), undefined,
                      binary(), binary(), binary()}.
-spec entry_to_item(rss_entry()) -> #item{}.
entry_to_item({entry, undefined, undefined, undefined,
               ID, undefined, Download, undefined, Category,
               Name, DateUploaded}) ->
    #item{id = ID,
          download_link = Download,
          name = Name,
          date_uploaded = DateUploaded,
          category = Category}.
