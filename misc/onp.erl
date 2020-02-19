-module(onp).
-author("piotr").

%% API
-export([onp/1]).

onp(X) when is_list(X) ->
  Tokens = string:tokens(X, " "),
  [Res] = parse_tokens(Tokens, []),
  Res;

onp(_) ->
  io:format("Improper data!~n").

is_this_string_a_float([]) -> false;

is_this_string_a_float([H | T]) ->
  case H of
    $. -> true;
    _ -> is_this_string_a_float(T)
  end.

parse_tokens([], Stack) ->
  Stack;

parse_tokens(["+" | T], [X, Y | TS]) ->
  parse_tokens(T, [X + Y | TS]);

parse_tokens(["-" | T], [X, Y | TS]) ->
  parse_tokens(T, [Y - X | TS]);

parse_tokens(["*" | T], [X, Y | TS]) ->
  parse_tokens(T, [X * Y | TS]);

parse_tokens(["/" | T], [X, Y | TS]) ->
  parse_tokens(T, [Y / X | TS]);

parse_tokens(["pow" | T], [X, Y | TS]) ->
  parse_tokens(T, [math:pow(Y, X) | TS]);

parse_tokens(["sqrt" | T], [X | TS]) ->
  parse_tokens(T, [math:sqrt(X) | TS]);

parse_tokens(["sin" | T], [X | TS]) ->
  parse_tokens(T, [math:sin(X) | TS]);

parse_tokens(["cos" | T], [X | TS]) ->
  parse_tokens(T, [math:cos(X) | TS]);

parse_tokens(["tan" | T], [X | TS]) ->
  parse_tokens(T, [math:tan(X) | TS]);

parse_tokens([H | T], Stack) ->
  case is_this_string_a_float(H) of
    false -> parse_tokens(T, [list_to_integer(H)] ++ Stack);
    true  -> parse_tokens(T, [list_to_float(H)] ++ Stack)
  end.