-module(myLists).
-author("piotr").

%% API
-export([contains/2]).
-export([duplicateElements/1]).
-export([sumFloats/1]).
-export([sumFloatsTailRec/2]).

contains([H | T], Value) ->
  case H of
    Value -> true;
    _ -> contains(T, Value)
  end;

contains([], _) -> false.

duplicateElements([]) -> [];

duplicateElements([H | T]) ->
  [H, H] ++ duplicateElements(T).

sumFloats([]) -> 0;

sumFloats([H | T]) when is_float(H) ->
  H + sumFloats(T);
sumFloats(_) ->
  io:format("Error, not float values in list~n").

sumFloatsTailRec([], Acc) -> Acc;

sumFloatsTailRec([H | T], Acc) when is_float(H) ->
  sumFloatsTailRec(T, Acc + H);

sumFloatsTailRec(_, _) ->
  io:format("Error, not float values in list~n").
