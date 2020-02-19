defmodule DataLoaderStream do
  @moduledoc false

  def import(filename \\ "pollution.csv") do
    File.stream!(filename) |> Stream.map(fn x -> String.split(x) end)
  end

  def parseOneLine(line) do
    [[day_date, hour, geo_x, geo_y, value]] = Enum.map(line, fn x ->  String.split(x, ",") end)
    day_date = String.split(day_date, "-") |> Enum.reverse() |> Enum.map(fn x -> Integer.parse(x) end) |>
      Enum.map(fn x -> elem(x, 0) end) |> :erlang.list_to_tuple()
    hour = String.split(hour, ":") |> Enum.map(fn x -> Integer.parse(x) end) |>
      Enum.map(fn x -> elem(x, 0) end) |> :erlang.list_to_tuple()
    geo_x = Float.parse(geo_x) |> elem(0)
    geo_y = Float.parse(geo_y) |> elem(0)
    value = Integer.parse(value) |> elem(0)
    %{:datetime => {day_date, hour}, :location => {geo_x, geo_y}, :pollutionLevel => value}
  end

  def parseAllLines(data) do
    Stream.map(data, fn x -> parseOneLine(x) end)
  end

  def identifyStations(dataList) do
    dataList |> Enum.reduce(%{}, fn el, acc -> Map.put(acc, el[:location], "") end)
  end

  def loadStations(stationMap) do
    :pollution_supervisor.start_link()
    for {k, _} <- stationMap, do:
      :pollution_server_gen.addStation("station_#{elem(k, 0)}_#{elem(k, 1)}", k)
  end

  def loadData(dataList) do
    listedInput = Enum.to_list(dataList)
    for el <- listedInput do
      :pollution_server_gen.addValue(el[:location], el[:datetime], "PM10", el[:pollutionLevel])
    end
  end

end
