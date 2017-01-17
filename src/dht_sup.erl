%%%-------------------------------------------------------------------
%% @doc dht top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(dht_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link([DHT_MODEL, GPIO, ClientPid]) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [DHT_MODEL, GPIO, ClientPid]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([DHT_MODEL, GPIO, ClientPid]) ->
    SupFlags = #{strategy => one_for_one,
                intensity => 1000,
                period => 5},
    ChildSpecs = [#{id => dht,
        start => {dht, start_link, [DHT_MODEL, GPIO, ClientPid]},
        shutdown => brutal_kill}],

    {ok, { SupFlags, ChildSpecs} }.

%%====================================================================
%% Internal functions
%%====================================================================
