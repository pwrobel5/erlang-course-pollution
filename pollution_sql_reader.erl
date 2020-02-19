-module(pollution_sql_reader).
-author("piotr").

%% API
-export([start_connection/0, read_stations/1, read_measurements/2, close_connection/1, demonstrate/0]).

start_connection() ->
  odbc:start(),
  {ok, Ref} = odbc:connect("DSN=erlang_pollution_db;UID=SA;PWD=erlang_project_2019", []),
  Ref.

add_station_data_to_server(Ref, StationIDs) ->
  {selected, _, Row} = odbc:next(Ref),
  case Row of
    [] -> StationIDs;
    _ ->
      [{StationID, Name, Coord_x, Coord_y}] = Row,
      pollution_server_gen:addStation(Name, {Coord_x, Coord_y}),
      add_station_data_to_server(Ref, StationIDs#{StationID => Name})
  end.

read_stations(Ref) ->
  {ok, _} = odbc:select_count(Ref, "select * from Stations"),
  add_station_data_to_server(Ref, #{}).

add_measurement_data_to_server(Ref, StationIDs) ->
  {selected, _, Row} = odbc:next(Ref),
  case Row of
    [] -> ok;
    _ ->
      [{_, Year, Month, Day, Hour, Minutes, Seconds, Type, Value, StationID}] = Row,
      #{StationID := StationName} = StationIDs,
      Time = {{Year, Month, Day}, {Hour, Minutes, Seconds}},
      pollution_server_gen:addValue(StationName, Time, Type, Value),
      add_measurement_data_to_server(Ref, StationIDs)
  end.

read_measurements(Ref, StationIDs) ->
  {ok, _} = odbc:select_count(Ref, "select * from Measurements"),
  add_measurement_data_to_server(Ref, StationIDs).

close_connection(Ref) ->
  odbc:disconnect(Ref),
  odbc:stop().

demonstrate() ->
  pollution_supervisor:start_link(),
  Ref = start_connection(),
  StationIDs = read_stations(Ref),
  read_measurements(Ref, StationIDs),
  close_connection(Ref),
  pollution_server_gen:crash(),
  timer:sleep(1000),
  pollution_monitor_keeper:get().
