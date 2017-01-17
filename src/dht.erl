%%%-------------------------------------------------------------------
%%% @author vasco
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Oct 2016 18:13
%%%-------------------------------------------------------------------
-module(dht).
-author("vasco").

-behaviour(gen_statem).

-include_lib("epigpio/include/epigpio.hrl").

-define(NAME, ?MODULE).

%% API
-export([start_link/3, read_sensor/0, decode_data/1]).

%% gen_statem callbacks
-export([
  init/1,
%%  format_status/2,
%%  state_name/3,
  handle_event/4,
  terminate/3,
  code_change/4,
  callback_mode/0
]).

-record(state, {dht_model,
                gpio,
                epigpio_pid,
                tick,
                data= <<>>,
                client_pid}).

%%%===================================================================
%%% API
%%%===================================================================

read_sensor() ->
  gen_statem:cast(?NAME, start).

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(DHT_MODEL, GPIO, ClientPid) ->
  lager:debug("--------> 1 <-----------"),
  gen_statem:start_link({local, ?NAME}, ?MODULE, [DHT_MODEL, GPIO, ClientPid], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {CallbackMode, StateName, State} |
%%                     {CallbackMode, StateName, State, Actions} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([DHT_MODEL, GPIO, ClientPid]) ->
%%  application:ensure_all_started(epigpio),

  epigpio_app:start(["pi3", 8888]),
  {ok, Pid} = epigpio_app_srv:get_epigpio_proc(self()),
  ok = epigpio:setmode(Pid, GPIO, ?PI_OUTPUT),
  ok = epigpio:setpullupdown(Pid, GPIO, ?PI_PUD_UP),
  ok = epigpio:notify_open(Pid),
  {ok, idle, #state{dht_model = DHT_MODEL, gpio = GPIO, epigpio_pid = Pid, client_pid = ClientPid}, infinity}.


callback_mode() ->
  handle_event_function.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Actions} |
%%                   {stop, Reason, NewState} |
%%    				 stop |
%%                   {stop, Reason :: term()} |
%%                   {stop, Reason :: term(), NewData :: data()} |
%%                   {stop_and_reply, Reason, Replies} |
%%                   {stop_and_reply, Reason, Replies, NewState} |
%%                   {keep_state, NewData :: data()} |
%%                   {keep_state, NewState, Actions} |
%%                   keep_state_and_data |
%%                   {keep_state_and_data, Actions}
%% @end
%%--------------------------------------------------------------------
handle_event(cast, start, idle, #state{epigpio_pid = Pid, gpio = GPIO} = State) ->
  lager:debug("GenStatem eventType: cast, Event: start, in StateName: idle, current State: ~p~n",[State]),
  %%  Start sequence (see https://www.itead.cc/wiki/images/3/39/AM2301_Manual.pdf)
  NextStateName = mcu_start_high,
  ok = epigpio:setmode(Pid, GPIO, ?PI_OUTPUT),
  ok = epigpio:write(Pid, GPIO, ?PI_LOW),
  ok = epigpio:mils(Pid, 1),
  Bits = 1 bsl GPIO,
  ok = epigpio:notify_begin(Pid, Bits),
  ok = epigpio:write(Pid, GPIO, ?PI_HIGH),
%%  ok = epigpio:mics(Pid, 50),
  ok = epigpio:tick(Pid),
  ok = epigpio:setmode(Pid, GPIO, ?PI_INPUT),
  {next_state, NextStateName, State#state{tick = 0}, infinity};

handle_event(info, {epigpio,{tick,0,0,Tick}}, StateName, State) ->
  lager:debug("Received Tick: ~p in State: ~p~n",[Tick, StateName]),
  {next_state, StateName, State#state{tick = Tick}, infinity};
handle_event(info, {epigpio, {notification, _Seq, _Flags, Tick, Level}} = Event, mcu_start_high, #state{epigpio_pid = Pid, gpio = GPIO} = State) ->
  lager:debug("Received eventType: info, Event: ~p, in StateName: mcu_start_high, Level: ~p~n",[Event, ((1 bsl GPIO) band Level bsr GPIO)]),
  {NextStateName, NewTick} = case ((1 bsl GPIO) band Level bsr GPIO) of
                               ?PI_HIGH -> {wait_start_response_low, Tick}; %%recived the front of the RESP_low signal, now waiting for the back (low-to-high)
                               _ -> {mcu_start_high, State#state.tick}%%Tick}
                             end,

  {next_state, NextStateName, State#state{tick = NewTick}, infinity};
handle_event(info, {epigpio, {notification, _Seq, _Flags, Tick, Level}} = Event, wait_start_response_low, #state{gpio = GPIO} = State) ->
  lager:debug("Received eventType: info, Event: ~p, in StateName: wait_start_response_low, current State: ~p~n",[Event, State]),
    {NextStateName, NewTick} = case ((1 bsl GPIO) band Level bsr GPIO) of
                                ?PI_LOW -> {wait_start_response_high, Tick}; %%recived the front of the RESP_low signal, now waiting for the back (low-to-high)
                                _ -> {wait_start_response_low, State#state.tick}
                              end,
 {next_state, NextStateName, State#state{tick = NewTick}, infinity};

handle_event(info, {epigpio, {notification, _Seq, _Flags, Tick, Level}} = Event, wait_start_response_high, #state{gpio = GPIO} = State) ->
  lager:debug("Received eventType: info, Event: ~p, in StateName: wait_start_response_high, current State: ~p~n",[Event, State]),
  Trel = Tick - State#state.tick ,
  lager:debug("--------> Trel = ~p",[Trel]),

  {next_state, wait_tlow_begin, State#state{tick = Tick}, infinity};
handle_event(info, {epigpio, {notification, _Seq, _Flags, Tick, Level}} = Event, wait_tlow_begin, #state{gpio = GPIO} = State) ->
  lager:debug("Received eventType: info, Event: ~p, in StateName: wait_tlow_begin, current State: ~p~n",[Event, State]),
  Treh = Tick - State#state.tick ,
  lager:debug("--------> Treh = ~p",[Treh]),
  {next_state, wait_tlow_end, State#state{tick = Tick}, infinity};
handle_event(info, {epigpio, {notification, _Seq, _Flags, Tick, Level}} = Event, wait_tlow_end, #state{gpio = GPIO, data = Data} = State) ->
  lager:debug("Received eventType: info, Event: ~p, in StateName: wait_tlow_end, current State: ~p~n",[Event, State]),
  {NextStateName, NewTick} = case ((1 bsl GPIO) band Level bsr GPIO) of
                               ?PI_HIGH -> Tlow = Tick - State#state.tick ,
                                 lager:debug("--------> Tlow = ~p",[Tlow]),
                                 case bit_size(Data) < 40  of
                                   true ->  {wait_data, Tick};
                                   false -> send_result(Data, State#state.client_pid),
                                            {idle, Tick}
                                 end;
                               _ -> {wait_tlow_end, Tick}%%State#state.tick}
                             end,
{next_state, NextStateName, State#state{tick = NewTick}, infinity};

handle_event(info, {epigpio, {notification, _Seq, _Flags, Tick, Level}} = Event, wait_data, #state{gpio = GPIO, data = Data} = State) ->
  lager:debug("Received eventType: info, Event: ~p, in StateName: wait_data, current State: ~p~n",[Event, State]),
  {Bit, NextStateName, NewTick} = case ((1 bsl GPIO) band Level bsr GPIO) of
                                    ?PI_LOW -> Tdata = Tick - State#state.tick ,
                                      lager:debug("--------> Tdata = ~p",[Tdata]),
                                      if
                                        Tdata =< 60 -> {<<0:1>>, wait_tlow_end, Tick};
                                        Tdata > 60 -> {<<1:1>>, wait_tlow_end, Tick};
                                        true -> {none, wait_data, State#state.tick}
                                      end;
                                    _ -> {none, wait_data, State#state.tick}
                                  end,
  NewData = case Bit of
              none -> Data;
              B -> <<Data/bitstring, B/bitstring>>
            end,
  {next_state, NextStateName, State#state{data = NewData, tick = NewTick}, infinity};
handle_event(info, {epigpio,{notify_open,0,0,0}} = Event, notify_open, #state{epigpio_pid = Pid, gpio = GPIO} = State) ->
  lager:debug("Received eventType: info, Event: ~p, in StateName: idle, current State: ~p~n",[Event, State]),
  Bits = 1 bsl GPIO,
  ok = epigpio:notify_begin(Pid, Bits),
  NextStateName = notify_begin,
  {next_state, NextStateName, State, infinity};
handle_event(info, {epigpio, {notify_begin, _, Bits, _}} = Event, notify_begin, #state{epigpio_pid = Pid, gpio = GPIO} = State) ->
  lager:debug("Received eventType: info, Event: ~p, in StateName: notify_begin, current State: ~p~n",[Event, State]),
  NextStateName = idle,
  {next_state, NextStateName, State, infinity};
handle_event(info, {start_notification, Pid}, StateName, State) ->
  lager:debug("Received eventType: ~p, Event: ~p, in StateName: ~p, current State: ~p~n",[info, start_notification, StateName, State]),
  ok = epigpio:notify_open(Pid),
  NextStateName = notify_open,
  {next_state, NextStateName, State, infinity};
handle_event(EventType, Event, StateName, State) ->
  lager:debug("Received unhandled eventType: ~p, Event: ~p, in StateName: ~p, current State: ~p~n",[EventType, Event, StateName, State]),
  {next_state, StateName, State, infinity}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
send_result(Data, Client) ->
  lager:debug("======> Sending result: ~p~n", [decode_data(Data)]),
  Client ! decode_data(Data).

decode_data(Data) ->
  <<H:16, T:16, Parity>> = Data,
  CheckSum = ((H bsr 8 ) + (H band 255) + (T bsr 8) + (T band 255)) band 255,
  case CheckSum =:= (Parity band 255) of
    true -> {H/10, T/10};
    _ -> {bad_result, Data, {H, T, CheckSum, Parity}}
  end.
