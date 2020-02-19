-module(pollution_server_sup).
-author("piotr").

%% API
-export([start_pollution_server/0]).

start_pollution_server() ->
  spawn(fun init_sup/0).

init_sup() ->
  process_flag(trap_exit, true),
  register(poll_server, spawn_link(pollution_server, init, [])),
  loop().

loop() ->
  receive
    {'EXIT', _, _} ->
      io:format("Restarting pollution server after crash~n"),
      register(poll_server, spawn_link(pollution_server, init, [])),
      loop()
  end.
