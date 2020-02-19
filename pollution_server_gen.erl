-module(pollution_server_gen).
-author("piotr").
-behaviour(gen_server).

%% API
-export([start/0, init/1, stop/0]).
-export([addStation/2, addValue/4, removeValue/3]).
-export([getOneValue/3, getStationMean/2, getDailyMean/2, getMaximumGrowthTime/3]).
-export([crash/0]).
-export([handle_cast/2, handle_call/3, terminate/2]).

%% user interface
start() ->
  InitialMonitor = pollution_monitor_keeper:get(),
  gen_server:start_link({local, poll_server}, ?MODULE, InitialMonitor, []).
init(Monitor) -> {ok, Monitor}.

addStation(Name, Coordinates) -> gen_server:cast(poll_server, {addStation, Name, Coordinates}).
addValue(ID, Time, Type, Value) -> gen_server:cast(poll_server, {addValue, ID, Time, Type, Value}).
removeValue(ID, Time, Type) -> gen_server:cast(poll_server, {removeValue, ID, Time, Type}).

getOneValue(Type, Date, ID) -> gen_server:call(poll_server, {getOneValue, Type, Date, ID}).
getStationMean(ID, Type) -> gen_server:call(poll_server, {getStationMean, ID, Type}).
getDailyMean(DayDate, Type) -> gen_server:call(poll_server, {getDailyMean, DayDate, Type}).
getMaximumGrowthTime(ID, Type, DayDate) -> gen_server:call(poll_server, {getMaximumGrowthTime, ID, Type, DayDate}).

stop() -> gen_server:call(poll_server, stop).
crash() -> gen_server:cast(poll_server, crash).


%% implementation


handle_cast({addStation, Name, Coordinates}, Monitor) ->
  {noreply, pollution:addStation(Name, Coordinates, Monitor)};

handle_cast({addValue, ID, Time, Type, Value}, Monitor) ->
  {noreply, pollution:addValue(ID, Time, Type, Value, Monitor)};

handle_cast({removeValue, ID, Time, Type}, Monitor) ->
  {noreply, pollution:removeValue(ID, Time, Type, Monitor)};

handle_cast(crash, Monitor) ->
  _CrashValue = 1 / 0,
  {noreply, Monitor}.

handle_call({getOneValue, Type, Date, ID}, _From, Monitor) ->
  Value = pollution:getOneValue(Type, Date, ID, Monitor),
  {reply, Value, Monitor};

handle_call({getStationMean, ID, Type}, _From, Monitor) ->
  Value = pollution:getStationMean(ID, Type, Monitor),
  {reply, Value, Monitor};

handle_call({getDailyMean, DayDate, Type}, _From, Monitor) ->
  Value = pollution:getDailyMean(DayDate, Type, Monitor),
  {reply, Value, Monitor};

handle_call({getMaximumGrowthTime, ID, Type, DayDate}, _From, Monitor) ->
  Value = pollution:getMaximumGrowthTime(ID, Type, DayDate, Monitor),
  {reply, Value, Monitor};

handle_call(stop, _From, Monitor) ->
  {stop, normal, ok, Monitor}.


terminate(Reason, Monitor) ->
  pollution_monitor_keeper:set(Monitor),
  io:format("Terminating pollution server~n"),
  Reason.
