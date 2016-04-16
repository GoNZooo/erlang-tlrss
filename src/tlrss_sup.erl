%%%-------------------------------------------------------------------
%% @doc tlrss top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(tlrss_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags = #{},
    Children = children(),
    {ok, {SupFlags, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
                                                % 
children() ->
    {ok, Feeds} = application:get_env(tlrss, feeds),
    {ok, Filters} = application:get_env(tlrss, filters),
    [#{id => tlrss_item_bucket,
       start => {tlrss_item_bucket, start_link, []}},
     #{id => tlrss_item_filter,
       start => {tlrss_item_filter, start_link, [Filters]}}].
