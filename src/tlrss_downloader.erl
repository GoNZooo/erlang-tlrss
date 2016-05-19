-module(tlrss_downloader).

-compile({parse_transform, do}).

-include("records.hrl").

-export([start_download/2,
         download/2]).

-spec download(feed, string()) -> types:error_m(string());
              (torrent, string()) -> types:error_m(binary()).
download(feed, Url) ->
    {ok, Pid} = tlrss_download_supervisor:start_child(feed, Url),
    get_data(Pid);
download(torrent, Url) ->
    {ok, Pid} = tlrss_download_supervisor:start_child(torrent, Url),
    get_data(Pid).

-spec start_download(feed, string()) -> {ok, pid()};
                    (torrent, string()) -> {ok, pid()}.
start_download(feed, Url) ->
    {ok, spawn_link(
           fun() -> do([error_m ||
                           Data <- download_feed(Url),
                           wait_for_fetch(Data)])
           end
          )};
start_download(torrent, Url) ->
    {ok, spawn_link(
           fun() -> do([error_m ||
                           Data <- download_torrent(Url),
                           wait_for_fetch(Data)])
           end
          )}.

-spec wait_for_fetch(Data :: string() | binary())
                    -> types:error_m(string() | binary()).
wait_for_fetch(Data) ->
    receive
        {From, get_data} ->
            From ! {ok, Data}
    after 5000 ->
            {error, "Data not claimed / timeout"}
    end.

-spec download_feed(string()) -> types:error_m([#item{}]).
download_feed(Url) ->
    do([error_m ||
           Data <- fetch_data(Url),
           RSSEntries <- try_parse_rss(Data),
           return(RSSEntries)
       ]).

-spec download_torrent(string()) -> types:error_m(binary()).
download_torrent(Url) ->
    do([error_m ||
           Data <- fetch_data(Url, binary),
           return(Data)
       ]).

-spec get_data(pid()) -> Data :: types:error_m(binary() | string()).
get_data(Pid) ->
    Pid ! {self(), get_data},
    receive
        {ok, Data} ->
            error_m:return(Data)

    after 30000 ->
            error_m:fail("Data not claimed / timeout")
    end.


-spec fetch_data(string()) -> types:error_m(string()).
fetch_data(Url) ->
    fetch_data(Url, string).

-spec fetch_data(string(), string) -> types:error_m(string());
                (string(), binary) -> types:error_m(binary()).
fetch_data(Url, string) ->
    do([error_m ||
           Result <- httpc:request(Url),
           {_, _, Data} = Result,
           return(Data)
       ]);
fetch_data(Url, binary) ->
    do([error_m ||
           {_, _, Data} <- httpc:request(get,
                                         {Url, []},
                                         [],
                                         [{body_format, binary}]),
           return(Data)
       ]).

-spec try_parse_rss(Data :: string()) -> Result :: types:error_m(#item{}).
try_parse_rss(Data) ->
    do([error_m ||
           RSSEntries <- do([error_m ||
                                try feeder:stream(Data, []) of
                                    {fatal_error, _, _, _, _} ->
                                        fail("Parsing error, invalid data");
                                    {ok, {_, Entries}, _} ->
                                        return(Entries)
                                catch
                                    error:_ ->
                                        fail("Invalid RSS data / Timeout")
                                end
                            ]),
           Items = lists:map(fun entry_to_item/1, RSSEntries),
           return(Items)
       ]).

-type rss_entry() :: {entry, _, _, _, ID :: binary(), _,
                      DownloadLink :: binary(), _, Category :: binary(),
                      Name :: binary(), DateUploaded :: binary()}.

-spec entry_to_item(rss_entry()) -> #item{}.
entry_to_item({entry, undefined, undefined, undefined,
               ID, undefined, Download, undefined, Category,
               Name, DateUploaded}) ->
    #item{id = ID,
          download_link = Download,
          name = Name,
          date_uploaded = DateUploaded,
          category = Category}.
