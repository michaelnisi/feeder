
-module(common).
-export([stream/1, file/1, is/2]).

is([],[]) ->
  ok;
is([AH|AT], [BH|BT]) ->
  etap:is(AH, BH, "should be equal"),
  is(AT, BT).


file(Filename) ->
  {ok, _, _} = feeder:file(Filename, opts()),
  receive_entries([]).

stream(Chunk) ->
  feeder:stream(Chunk, opts()),
  receive_entries([]).

opts() ->
  [{event_state, {}}, {event_fun, fun event/2}].

receive_entries(Entries) ->
  receive
    {entry, Entry} ->
      receive_entries([Entry|Entries]);
    endFeed ->
      Entries
  end.

event({entry, Entry}, S) ->
  self() ! {entry, Entry},
  S;
event(endFeed, S) ->
self() ! endFeed,
  S;
event(_, S) ->
  S.
