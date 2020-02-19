-module(pollution_server_tests).
-author("piotr").
-include_lib("eunit/include/eunit.hrl").

pollution_server_test() ->
  pollution_server:start(),
  pollution_server:addStation("Telimeny", {10.0, 10.0}),
  {DayDate, _} = calendar:local_time(),
  pollution_server:addValue({10.0, 10.0}, {DayDate, {00,05,06}}, "PM10", 12.0),
  pollution_server:addValue({10.0, 10.0}, {DayDate, {10,15,08}}, "PM10", 20.0),
  pollution_server:addValue({10.0, 10.0}, {DayDate, {12,25,06}}, "PM10", 42.0),
  pollution_server:addValue({10.0, 10.0}, {DayDate, {20,05,06}}, "PM10", 120.0),
  pollution_server:addValue({10.0, 10.0}, {{2019,03,04}, {00,05,06}}, "PM10", 10.0),
  pollution_server:addValue({10.0, 10.0}, {{2019,03,04}, {10,05,06}}, "PM10", 30.0),
  pollution_server:addValue("Telimeny", {DayDate, {12,00,07}}, "PM2.5", 40.0),
  pollution_server:addValue("Telimeny", {DayDate, {11,30,07}}, "PM2.5", 10.0),
  pollution_server:addValue("Telimeny", {{2019,03,04}, {12,00,04}}, "PM2.5", 13.0),
  pollution_server:removeValue({10.0, 10.0}, {DayDate, {12,00,07}}, "PM2.5"),
  ?assertMatch(10.0, pollution_server:getOneValue("PM2.5", {DayDate, {11,30,07}}, {10.0, 10.0})),
  ?assertMatch(12.0, pollution_server:getOneValue("PM10", {DayDate, {00,05,06}}, "Telimeny")),
  ?assertMatch(39.0, pollution_server:getStationMean("Telimeny", "PM10")),
  ?assertMatch(11.5, pollution_server:getStationMean("Telimeny", "PM2.5")),
  ?assertMatch(48.5, pollution_server:getDailyMean(DayDate, "PM10")),
  ?assertMatch(10.0, pollution_server:getDailyMean(DayDate, "PM2.5")),
  ?assertMatch(20.0, pollution_server:getDailyMean({2019,03,04}, "PM10")),
  ?assertMatch(13.0, pollution_server:getDailyMean({2019,03,04}, "PM2.5")),
  ?assertMatch({20,05,06}, pollution_server:getMaximumGrowthTime("Telimeny", "PM10", DayDate)),
  ?assertMatch({10,05,06}, pollution_server:getMaximumGrowthTime("Telimeny", "PM10", {2019,03,04})),
  pollution_server:stop().
