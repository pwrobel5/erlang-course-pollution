-module(pingpong).
-author("piotr").

%% API
-export([start/0]).
-export([stop/0]).
-export([play/1]).
-export([ping_loop/1]).
-export([pong_loop/0]).

ping_loop(State) ->
  receive
    0 -> ping_loop(State);
    terminate -> io:format("Terminating ping~n");
    N -> io:format("Ping received ~p, current sum of received numbers:~p~n", [N, State]),
      timer:sleep(1000),
      pong ! N - 1,
      ping_loop(State + N)
  after
    20000 -> io:format("20 seconds passed, terminating ping~n")
  end.

pong_loop() ->
  receive
    0 -> pong_loop();
    terminate -> io:format("Terminating pong~n");
    N -> io:format("Pong received ~p~n", [N]),
      timer:sleep(1000),
      ping ! N - 1,
      pong_loop()
  after
    20000 -> io:format("20 seconds passed, terminating pong~n")
  end.

start() ->
  Ping_pid = spawn(?MODULE, ping_loop, [0]),
  register(ping, Ping_pid),
  Pong_pid = spawn(?MODULE, pong_loop, []),
  register(pong, Pong_pid).

stop() ->
  ping ! terminate,
  pong ! terminate.

play(N) ->
  ping ! N.
