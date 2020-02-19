-module(pollution_server).
-author("piotr").

%% API
-export([start/0]).
-export([stop/0]).

-export([addStation/2]).
-export([addValue/4]).
-export([removeValue/3]).
-export([getOneValue/3]).
-export([getStationMean/2]).
-export([getDailyMean/2]).
-export([getMaximumGrowthTime/3]).
-export([crash/0]).
-export([init/0]).

init() ->
  BlankMonitor = pollution:createMonitor(),
  loop(BlankMonitor).

start() ->
  register(poll_server, spawn(fun() -> init() end)).

stop() ->
  poll_server ! stop.

loop(State) ->
  receive
    {request, Pid, {addStation, Name, Coordinates}} ->
      Pid ! {reply, ok},
      loop(pollution:addStation(Name, Coordinates, State));
    {request, Pid, {addValue, ID, Time, Type, Value}} ->
      Pid ! {reply, ok},
      loop(pollution:addValue(ID, Time, Type, Value, State));
    {request, Pid, {removeValue, ID, Time, Type}} ->
      Pid ! {reply, ok},
      loop(pollution:removeValue(ID, Time, Type, State));
    {request, Pid, {getOneValue, Type, Date, ID}} ->
      Value = pollution:getOneValue(Type, Date, ID, State),
      Pid ! {reply, Value},
      loop(State);
    {request, Pid, {getStationMean, ID, Type}} ->
      Value = pollution:getStationMean(ID, Type, State),
      Pid ! {reply, Value},
      loop(State);
    {request, Pid, {getDailyMean, DayDate, Type}} ->
      Value = pollution:getDailyMean(DayDate, Type, State),
      Pid ! {reply, Value},
      loop(State);
    {request, Pid, {getMaximumGrowthTime, ID, Type, DayDate}} ->
      Value = pollution:getMaximumGrowthTime(ID, Type, DayDate, State),
      Pid ! {reply, Value},
      loop(State);
    {request, _Pid, crash} ->
      _CrashVal = 1 / 0;
    stop -> ok
  end.

%% client

call(Message) ->
  poll_server ! {request, self(), Message},
  receive
    {reply, Reply} -> Reply
  end.

addStation(Name, Coordinates) -> call({addStation, Name, Coordinates}).
addValue(ID, Time, Type, Value) -> call({addValue, ID, Time, Type, Value}).
removeValue(ID, Time, Type) -> call({removeValue, ID, Time, Type}).
getOneValue(Type, Date, ID) -> call({getOneValue, Type, Date, ID}).
getStationMean(ID, Type) -> call({getStationMean, ID, Type}).
getDailyMean(DayDate, Type) -> call({getDailyMean, DayDate, Type}).
getMaximumGrowthTime(ID, Type, DayDate) -> call({getMaximumGrowthTime, ID, Type, DayDate}).
crash() -> poll_server ! {request, self(), crash}.