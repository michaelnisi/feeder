-module(feeder_test).
-compile(export_all).

-include_lib("etest/include/etest.hrl").

test_item() ->
  Opts = [{event_state, {}}, {event_fun, fun event/2}], 
  feeder:stream("<channel><item><title>Nice Title</title></item><item><title>Nice Title</title></item></channel>", Opts),
  Entries = receive_entries([]),
  [?assert_equal(<<"Nice Title">>, Title) || {Title, _, _, _} <- Entries],
  ?assert_equal(2, length(Entries)).

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
