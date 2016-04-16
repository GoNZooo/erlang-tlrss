-module(tlrss_item_bucket).
-export([add/1,
         items/0]).

-export([start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-behaviour(gen_server).
-include("records.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add(Item) ->
    gen_server:call(?MODULE, {add, Item}).

items() ->
    gen_server:call(?MODULE, items).

% Internals

init([]) ->
    {ok, maps:new()}.

handle_call({add, AddedItems}, _From, OldItems) ->
    NewItems = lists:filter(fun(I) -> item_is_new(I, OldItems) end, AddedItems),
    CombinedItems = add_items_to_map(NewItems, OldItems),
    {reply, {new_items, NewItems}, CombinedItems};
handle_call(items, _From, Items) ->
    {reply, {items, Items}, Items}.

add_items_to_map(Items, Map) ->
    NewItems = lists:map(fun(I) -> {I#item.name, I} end, Items),
    NewItemsMap = maps:from_list(NewItems),
    maps:merge(Map, NewItemsMap).

item_is_new(Item, Items) ->
    not maps:is_key(Item#item.name, Items).

terminate(_Reason, _N) ->
    ok.

handle_cast(_Msg, N) ->
    {noreply, N}.

handle_info(_Info, N) ->
    {noreply, N}.

code_change(_OldVsn, N, _Extra) ->
    {ok, N}.
