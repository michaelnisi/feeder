%%
%% feeder_tuples - modify tuples
%%

-module(feeder_tuples).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([trim/1]).
-export([update/3]).
-export([append/3]).

trim(S) ->
  Bin = unicode:characters_to_binary(S, utf8),
  RE = "^[ \t\n\r]+|[ \t\n\r]+$",
  re:replace(Bin, RE, "", [global, {return, binary}]).

%% Once we deliberately set something, we do not override it.
update(T, F, L) when L =/= [] ->
  case element(F, T) of
    undefined -> setelement(F, T, trim(L));
    _ -> T
  end;
update(T, _, _) ->
  T.

%% Some fields are lists (e.g. category nodes)
append(T, F, L) when L =/= [] ->
  case element(F, T) of
    undefined -> setelement(F, T, [trim(L)]);
    E -> setelement(F, T, [trim(L)|E])
  end;
append(T, _, _) ->
  T.

-ifdef(TEST).

trim_test_() -> [
  ?_assertMatch(<<"">>, trim("")),
  ?_assertMatch(<<"hello">>, trim(" hello "))
].

-endif.
