-module(tlrss_item_filter).
-export([filter/1,
         filters/0]).

-export([start_link/1,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-behaviour(gen_server).
-include("records.hrl").

start_link(Filters) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Filters, []).

filter(Items) ->
    gen_server:call(?MODULE, {filter, Items}).

filters() ->
    gen_server:call(?MODULE, filters).

init(Filters) ->
    Compiled = lists:map(fun({F, Opts}) ->
                                 {ok, Regex} = re:compile(F, Opts),
                                 Regex
                         end, Filters),
    {ok, Compiled}.

handle_call({filter, Items}, _From, Filters) ->
    Filtered = lists:filter(fun(I) -> wanted_item(I, Filters) end, Items),
    {reply, {filtered_items, Filtered}, Filters};
handle_call(filters, _From, Filters) ->
    {reply, {filters, Filters}, Filters}.

wanted_item(Item, Filters) ->
    lists:any(fun(Regex) ->
                      case re:run(Item#item.name, Regex) of
                          {match, _} -> true;
                          _ -> false
                      end
              end, Filters).

handle_cast(_Msg, N) ->
    {noreply, N}.

handle_info(_Info, N) ->
    {noreply, N}.

terminate(_Reason, _N) ->
    ok.

code_change(_OldVsn, N, _Extra) ->
    {ok, N}.
