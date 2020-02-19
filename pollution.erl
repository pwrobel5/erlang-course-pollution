-module(pollution).
-author("piotr").

%% API
-export([createMonitor/0]).
-export([addStation/3]).
-export([addValue/5]).
-export([removeValue/4]).
-export([getOneValue/4]).
-export([getStationMean/3]).
-export([getDailyMean/3]).
-export([getMaximumGrowthTime/4]).

-record(station, {name, data = #{}}).

createMonitor() ->
  #{}.

isNameTaken(Name, Monitor) ->
  case map_size(maps:filter(fun(_, V) -> V#station.name == Name end, Monitor)) of
    0 -> false;
    _   -> true
  end.

doesThisMeasureExist(Data, Time, Type) ->
  case map_size(maps:filter(fun(K, V) -> (K == Time) andalso (maps:is_key(Type, V)) end, Data)) of
    0 -> false;
    _ -> true
  end.

getStationCoordinatesByName(Name, Monitor) ->
  maps:keys(maps:filter(fun (_,V) -> V#station.name == Name end, Monitor)).

addStation(Name, {Geo_x, Geo_y}, Monitor) ->
  case {maps:is_key({Geo_x, Geo_y}, Monitor), isNameTaken(Name, Monitor)} of
    {false, false} -> Monitor#{{Geo_x, Geo_y} => #station{name = Name}};
    _              -> {Monitor, throw("The station already exists!")}
  end.

addValue({Geo_x, Geo_y}, Time, Type, Value, Monitor) ->
  case maps:is_key({Geo_x, Geo_y}, Monitor) of
    false -> {Monitor, throw("Station does not exist!")};
    true  ->
      Coordinates = {Geo_x, Geo_y},
      #{Coordinates := #station{name = Name, data = Data}} = Monitor,
      case {doesThisMeasureExist(Data, Time, Type), maps:is_key(Time, Data)} of
        {true, _} -> {Monitor, throw("Measurement already exists!")};
        {false, true} ->
          #{Time := Measures} = Data,
          Monitor#{Coordinates := #station{name = Name, data = Data#{Time := Measures#{Type => Value}}}};
        {false, false} ->
          Monitor#{Coordinates := #station{name = Name, data = Data#{Time => #{Type => Value}}}}
      end
  end;

addValue(Name, Time, Type, Value, Monitor) ->
  case isNameTaken(Name, Monitor) of
    false -> {Monitor, throw("Station does not exist!")};
    true  ->
      [Coordinates] = getStationCoordinatesByName(Name, Monitor),
      addValue(Coordinates, Time, Type, Value, Monitor)
  end.

removeValue({Geo_x, Geo_y}, Time, Type, Monitor) ->
  Coordinates = {Geo_x, Geo_y},
  #{Coordinates := #station{name = Name, data = Data}} = Monitor,
  #{Time := Measures} = Data,
  NewMeasures = maps:remove(Type, Measures),
  case map_size(NewMeasures) of
    0 -> Monitor#{Coordinates := #station{name = Name, data = maps:remove(Time, Data)}};
    _ -> Monitor#{Coordinates := #station{name = Name, data = Data#{Time := NewMeasures}}}
  end;

removeValue(Name, Time, Type, Monitor) ->
  [Coordinates] = getStationCoordinatesByName(Name, Monitor),
  removeValue(Coordinates, Time, Type, Monitor).

getOneValue(Type, Date, {Geo_x, Geo_y}, Monitor) ->
  Coordinates = {Geo_x, Geo_y},
  #{Coordinates := #station{name = _, data = #{Date := #{Type := Value}}}} = Monitor,
  Value;

getOneValue(Type, Date, Name, Monitor) ->
  [Coordinates] = getStationCoordinatesByName(Name, Monitor),
  getOneValue(Type, Date, Coordinates, Monitor).

getStationMean({Geo_x, Geo_y}, Type, Monitor) ->
  Coordinates = {Geo_x, Geo_y},
  #{Coordinates := #station{name = _, data = StationData}} = Monitor,
  TypeData = maps:filter(fun (_, V) -> maps:is_key(Type, V) end, StationData),
  Sum = maps:fold(fun (_, V, AccIn) -> #{Type := Value} = V, AccIn + Value end, 0, TypeData),
  NumberOfMeasures = map_size(TypeData),
  Sum / NumberOfMeasures;

getStationMean(Name, Type, Monitor) ->
  [Coordinates] = getStationCoordinatesByName(Name, Monitor),
  getStationMean(Coordinates, Type, Monitor).

getDailyMean(DayDate, Type, Monitor) ->
  DayMonitor = maps:fold(fun (K, V, AccIn) ->
    #station{name = Name, data = StationData} = V,
    FilteredStationData = maps:filter(fun (K1, V1) ->
      case {K1, maps:is_key(Type, V1)} of
        {{DayDate, _}, true} -> true;
        _ -> false
      end end, StationData),
    case map_size(FilteredStationData) of
      0 -> AccIn;
      _ -> AccIn#{K => #station{name = Name, data = FilteredStationData}}
    end end,
      #{}, Monitor),
  DailySum = maps:fold(fun (_, V, AccIn) ->
    #station{name = _, data = StationData} = V,
    AccIn + maps:fold(fun (_, V1, AccIn1) ->
      case maps:is_key(Type, V1) of
        true -> AccIn1 + maps:get(Type, V1);
        _ -> AccIn1
      end end,
      0, StationData) end,
    0, DayMonitor),
  NumberOfMeasures = maps:fold(fun (_, V, AccIn) ->
    #station{name = _, data = StationData} = V,
    AccIn + map_size(StationData) end,
    0, DayMonitor),
  DailySum / NumberOfMeasures.

getMaximumGrowthTime({Geo_x, Geo_y}, Type, DayDate, Monitor) ->
  Coordinates = {Geo_x, Geo_y},
  #{Coordinates := #station{name = _, data = StationData}} = Monitor,
  DayData = maps:filter(fun (K, V) ->
    case {K, maps:is_key(Type, V)} of
      {{DayDate, _}, true} -> true;
      _ -> false
    end end, StationData),
  TempValueList = lists:sort(maps:to_list(DayData)),
  ValueList = [{Hour, maps:get(Type, Map)} || {{_, Hour}, Map} <- TempValueList],
  [ValueListHead | ValueListTail] = ValueList,
  {ValueListHeadHour, _} = ValueListHead,
  GrowthList = [{Hour, Curr - Prev} || {Hour, Curr} <- ValueListTail, {_, Prev} <- ValueList],
  FullGrowthList = [{ValueListHeadHour, 0}] ++ GrowthList,
  MaxGrowth = lists:foldr(fun
                ({Hour, Value}, {_, AccVal}) when Value > AccVal -> {Hour, Value};
                ({_, Value}, {AccH, AccVal}) when Value =< AccVal -> {AccH, AccVal} end, {0, 0}, FullGrowthList),
  case MaxGrowth of
    {0, 0} -> io:format("No growth detected in this day and station~n");
    {MaxGrowthHour, _} -> MaxGrowthHour
  end;

getMaximumGrowthTime(Name, Type, DayDate, Monitor) ->
  [Coordinates] = getStationCoordinatesByName(Name, Monitor),
  getMaximumGrowthTime(Coordinates, Type, DayDate, Monitor).