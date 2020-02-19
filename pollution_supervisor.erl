-module(pollution_supervisor).
-author("piotr").
-behaviour(supervisor).

%% API
-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, poll_supervisor}, ?MODULE, []).

init(_InitValue) ->
  {ok, {
    {one_for_one, 1, 2},
    [
      {pollution_monitor_kpr,
        {pollution_monitor_keeper, start, [#{}]},
        permanent, brutal_kill, worker, [pollution_monitor_keeper]},
      {poll_server_sup,
        {pollution_server_gen, start, []},
        permanent, brutal_kill, worker, [pollution_server_gen]}
    ]}
    }.
