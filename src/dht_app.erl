%%%-------------------------------------------------------------------
%% @doc dht public API
%% @end
%%%-------------------------------------------------------------------

-module(dht_app).

-behaviour(application).

%% Application callbacks
-export([start/2, start/3, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(DHT_MODEL, GPIO, ClientPid)->

    start(normal, [DHT_MODEL, GPIO, ClientPid]).

start(_StartType, [DHT_MODEL, GPIO, ClientPid]) ->
    lager:debug([{module, dht_app}], "dht_app:start: ~p,~p,~p~n",[DHT_MODEL, GPIO, ClientPid]),
    dht_sup:start_link([DHT_MODEL, GPIO, ClientPid]).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
