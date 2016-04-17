-module(tlrss_downloader).

-export([get_data/1,
         download_wait/2]).

-include("records.hrl").

-export([start_link/2,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-behaviour(gen_server).

start_link(feed, Url) ->
    gen_server:start_link(?MODULE, [feed, Url], []);
start_link(torrent, Url) ->
    gen_server:start_link(?MODULE, [torrent, Url], []).

download_wait(feed, Url) ->
    {ok, Pid} = tlrss_download_supervisor:start_child(feed, Url),
    get_data(Pid);
download_wait(torrent, Url) ->
    {ok, Pid} = tlrss_download_supervisor:start_child(torrent, Url),
    get_data(Pid).

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

get_data(Pid) ->
    gen_server:call(Pid, get_data).

entry_to_item({entry, undefined, undefined, undefined,
               ID, undefined, Download, undefined, Category,
               Name, DateUploaded}) ->
    #item{id = ID,
          download_link = Download,
          name = Name,
          date_uploaded = DateUploaded,
          category = Category}.

init([feed, Url]) ->
    {ok, download_feed(Url)};
init([torrent, Url]) ->
    {ok, download_torrent(Url)}.


handle_call(get_data, _From, Data) ->
    {reply, Data, Data}.

handle_cast(_Msg, N) ->
    {noreply, N}.

handle_info(_Info, N) ->
    {noreply, N}.

terminate(_Reason, _N) ->
    ok.

code_change(_OldVsn, N, _Extra) ->
    {ok, N}.
