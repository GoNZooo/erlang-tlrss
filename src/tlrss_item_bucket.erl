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

-spec start_link() -> gen_server:startlink_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec add([#item{}]) -> {ok, [#item{}]}.
add(Items) ->
    gen_server:call(?MODULE, {add, Items}).

-spec items() -> {items, [#item{}]}.
items() ->
    gen_server:call(?MODULE, items).

% Internals

init([]) ->
    {ok, maps:new()}.

-spec add_items_to_map([#item{}], #{}) -> #{}.
add_items_to_map(Items, Map) ->
    NewItems = lists:map(fun(I) -> {I#item.name, I} end, Items),
    NewItemsMap = maps:from_list(NewItems),
    maps:merge(Map, NewItemsMap).

-spec item_is_new(#item{}, #{}) -> boolean().
item_is_new(Item, Items) ->
    not maps:is_key(Item#item.name, Items).

handle_call({add, AddedItems}, _From, OldItems) ->
    NewItems = lists:filter(fun(I) -> item_is_new(I, OldItems) end, AddedItems),
    CombinedItems = add_items_to_map(NewItems, OldItems),

    {reply, {ok, NewItems}, CombinedItems};
handle_call(items, _From, Items) ->

    {reply, {items, Items}, Items}.

handle_cast(_Msg, N) ->
    {noreply, N}.

handle_info(_Info, N) ->
    {noreply, N}.

terminate(_Reason, _N) ->
    ok.

code_change(_OldVsn, N, _Extra) ->
    {ok, N}.
