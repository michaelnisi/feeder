#! /usr/bin/env escript

-mode(compile).
-include("../include/feeder.hrl").

main([]) ->
  code:add_pathz("test"),
  code:add_pathz("ebin"),
  etap:plan(4),
  test(),
  etap:is(1,1, "should be one"),
  etap:end_tests().

test() ->
  Opts = [{event_state, {}}, {event_fun, fun event/2}], 
  feeder:stream("<channel><item><title>Nice Title</title></item><item><title>Nice Title</title></item></channel>", Opts),
  Entries = receive_entries([]),
  Entry = #entry{title = <<"Nice Title">>},
  [etap:is(E, Entry,  "should be the same") || E <- Entries],
  etap:is(2, length(Entries), "should be 2"),
  ok.

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
