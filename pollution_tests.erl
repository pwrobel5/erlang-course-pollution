-module(pollution_tests).
-author("piotr").
-include_lib("eunit/include/eunit.hrl").

-record(station, {name, data = #{}}).

createMonitor_test() ->
  ?assertMatch(#{}, pollution:createMonitor()).


addStation_test_() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Telimeny", {10.0, 10.0}, P),
  [
    ?_assertMatch(#{{10.0, 10.0} := #station{name = "Telimeny"}}, P1),
    ?_assertError(_, pollution:addStation({10.5, 11.0}, "Aleja Pokoju", P)),
    ?_assertThrow(_, pollution:addStation("Telimeny", {20.0, 20.0}, P1)),
    ?_assertThrow(_, pollution:addStation("Aleksandry", {10.0, 10.0}, P1))
  ].

addValue_test_() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Telimeny", {10.0, 10.0}, P),
  Time = calendar:local_time(),
  P2 = pollution:addValue({10.0, 10.0}, Time, "PM10", 10.0, P1),
  [
    ?_assertMatch(#{{10.0, 10.0} := #station{name = "Telimeny", data = #{Time := #{"PM10" := 10.0}}}},
      P2),
    ?_assertThrow(_, pollution:addValue({11.0, 22.0}, Time, "PM10", 11.5, P1)),
    ?_assertThrow(_, pollution:addValue({10.0, 10.0}, Time, "PM10", 111.2, P2)),
    ?_assertMatch(#{{10.0, 10.0} := #station{name = "Telimeny", data = #{Time := #{"PM10" := 10.0, "PM2.5" := 40.0}}}},
      pollution:addValue("Telimeny", Time, "PM2.5", 40.0, P2)),
    ?_assertThrow(_, pollution:addValue("Telimeny", Time, "PM10", 11.2, P2))
  ].

removeValue_test_() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Telimeny", {10.0, 10.0}, P),
  Time = calendar:local_time(),
  P2 = pollution:addValue({10.0, 10.0}, Time, "PM10", 10.0, P1),
  P3 = pollution:addValue("Telimeny", Time, "PM2.5", 40.0, P2),
  [
    ?_assertMatch(P2, pollution:removeValue({10.0, 10.0}, Time, "PM2.5", P3)),
    ?_assertMatch(P3, pollution:removeValue("Telimeny", Time, "Temp", P3))
  ].

getOneValue_test_() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Telimeny", {10.0, 10.0}, P),
  Time = calendar:local_time(),
  P2 = pollution:addValue({10.0, 10.0}, Time, "PM10", 10.0, P1),
  P3 = pollution:addValue("Telimeny", Time, "PM2.5", 40.0, P2),
  [
    ?_assertMatch(40.0, pollution:getOneValue("PM2.5", Time, {10.0, 10.0}, P3)),
    ?_assertMatch(10.0, pollution:getOneValue("PM10", Time, "Telimeny", P3)),
    ?_assertError(_, pollution:getOneValue("Temp", Time, "Telimeny", P3))
  ].

getStationMean_test_() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Telimeny", {10.0, 10.0}, P),
  Time = calendar:local_time(),
  P2 = pollution:addValue({10.0, 10.0}, Time, "PM10", 10.0, P1),
  {Day, _} = calendar:local_time(),
  Time2 = {Day, {11,08,09}},
  P3 = pollution:addValue("Telimeny", Time2, "PM10", 40.0, P2),
  [
    ?_assertMatch(25.0, pollution:getStationMean({10.0, 10.0}, "PM10", P3)),
    ?_assertMatch(25.0, pollution:getStationMean("Telimeny", "PM10", P3)),
    ?_assertError(_, pollution:getStationMean("Aleksandry", "PM2.5", P3))
  ].

getDailyMean_test_() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Telimeny", {10.0, 10.0}, P),
  Time = calendar:local_time(),
  {DayTime, _} = Time,
  P2 = pollution:addValue({10.0, 10.0}, Time, "PM10", 12.0, P1),
  P3 = pollution:addValue("Telimeny", {DayTime, {11,08,09}}, "PM10", 40.0, P2),
  Time2 = {{2019,6,2}, {23,52,8}},
  P4 = pollution:addStation("Aleksandry", {20.0, 20.0}, P3),
  P5 = pollution:addValue("Aleksandry", Time2, "PM10", 15.0, P4),
  P6 = pollution:addValue("Aleksandry", Time, "PM10", 14.0, P5),
  [
    ?_assertMatch(22.0, pollution:getDailyMean(DayTime, "PM10", P6)),
    ?_assertMatch(15.0, pollution:getDailyMean({2019,6,2}, "PM10", P6))
  ].

getMaximumGrowthTime_test_() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Telimeny", {10.0, 10.0}, P),
  {DayTime, _} = calendar:local_time(),
  P2 = pollution:addValue({10.0, 10.0}, {DayTime, {00,05,06}}, "PM10", 12.0, P1),
  P3 = pollution:addValue("Telimeny", {DayTime, {11,08,09}}, "PM10", 40.0, P2),
  Time2 = {{2019,6,2}, {23,52,8}},
  P4 = pollution:addValue("Telimeny", Time2, "PM10", 15.0, P3),
  P5 = pollution:addValue("Telimeny", {DayTime, {10,05,02}}, "PM10", 14.0, P4),
  [
    ?_assertMatch({11,08,09}, pollution:getMaximumGrowthTime({10.0, 10.0}, "PM10", DayTime, P5)),
    ?_assertMatch({11,08,09}, pollution:getMaximumGrowthTime("Telimeny", "PM10", DayTime, P5))
  ].
