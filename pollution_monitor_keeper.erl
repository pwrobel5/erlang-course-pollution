-module(pollution_monitor_keeper).
-author("piotr").
-behaviour(gen_server).

%% API
-export([start/1, init/1, set/1, get/0, stop/0]).
-export([handle_cast/2, handle_call/3, terminate/2]).

start(InitialMonitor) -> gen_server:start_link({local, monitor_keeper}, ?MODULE, InitialMonitor, []).
init(Monitor) -> {ok, Monitor}.

set(NewMonitor) -> gen_server:cast(monitor_keeper, {set, NewMonitor}).
get() -> gen_server:call(monitor_keeper, get).
stop() -> gen_server:call(monitor_keeper, stop).

handle_cast({set, NewMonitor}, _Monitor) -> {noreply, NewMonitor}.

handle_call(get, _From, Monitor) -> {reply, Monitor, Monitor};
handle_call(stop, _From, Monitor) -> {stop, normal, ok, Monitor}.

terminate(Reason, _Monitor) ->
  io:format("Terminating Monitor keeper~n"),
  Reason.
