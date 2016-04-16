-module(tlrss_download_feed).
-export([download_feed/1]).

-include("records.hrl").

download_feed(Url) ->
    Entries = stream(Url),
    lists:map(fun entry_to_item/1, Entries).

get_data(Url) ->
    {ok, {_, _, Data}} = httpc:request(Url),
    Data.

stream(Url) ->
    {ok, {_, Entries}, _Rest} = feeder:stream(get_data(Url), []),
    Entries.

entry_to_item({entry, undefined, undefined, undefined,
               ID, undefined, Download, undefined, Category,
               Name, DateUploaded}) ->
    #item{id = ID,
          download_link = Download,
          name = Name,
          date_uploaded = DateUploaded,
          category = Category}.
