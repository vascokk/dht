%%%-------------------------------------------------------------------
%%% @author vasco
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Oct 2016 19:42
%%%-------------------------------------------------------------------
-module(dht_tests).
-author("vasco").

-compile([debug_info, export_all]).
-compile([{parse_transform, lager_transform}]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epigpio/include/epigpio.hrl").

-define(assertReceived(T),
  receive
    T -> ?assert(true)
  after
    50000 -> ?assert(false)
  end).

simple_test() ->
  ?assert(true).

decode_data_test() ->
%%  0000 0010 1001 0010 0000 0001 0000 1101 1010 0010 => H=65.8, T=26.9,
%%  Data = "0000001010010010000000010000110110100010",
  Data = <<0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 1:1, 0:1, 1:1, 0:1, 0:1, 1:1, 0:1, 0:1, 1:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 1:1, 0:1, 0:1, 0:1, 0:1, 1:1, 1:1, 0:1, 1:1, 1:1, 0:1, 1:1, 0:1, 0:1, 0:1, 1:1, 0:1>>,
  Res = dht:decode_data(Data),
  ?assertEqual({65.8, 26.9}, Res).


read_sensor_test_() ->
  {timeout, 10,
    fun() ->
      read_sensor()
%%      start_seq()
    end}.

start_seq() ->
  lager:start(),
  lager:set_loglevel(lager_console_backend, debug),
  GPIO = 4,
  epigpio_app:start(["pi3", 8888]),
  {ok, Pid} = epigpio_app_srv:get_epigpio_proc(self()),
%%  {ok, Pid} = dht:start_link(dht21, GPIO, self()),
  timer:sleep(1000),
  ok = epigpio:setmode(Pid, GPIO, ?PI_OUTPUT),
  ok = epigpio:setpullupdown(Pid, GPIO, ?PI_PUD_UP),
  %%ok = epigpio:write(Pid, GPIO, ?PI_HIGH),
  ok = epigpio:write(Pid, GPIO, ?PI_LOW),
  ok = epigpio:mils(Pid, 15),
  ok = epigpio:write(Pid, GPIO, ?PI_HIGH),
  ok = epigpio:mics(Pid, 50),
  ok = epigpio:setmode(Pid, GPIO, ?PI_INPUT).
%%  timer:sleep(20000).

read_sensor() ->
  lager:start(),

  lager:set_loglevel(lager_console_backend, debug),
  GPIO = 4,
  {ok, _} = dht_app:start(dht21, GPIO, self()),
%%(1 bsl 4 ) band 536922623 bsr 4.
  timer:sleep(1000),
  dht:read_sensor(),
  ?assertReceived({H, T}).