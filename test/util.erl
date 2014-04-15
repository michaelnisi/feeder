
-module(util).
-export([file/1]).

event({entry, Entry}, {Feed, Entries}) ->
  {Feed, [Entry|Entries]};
event({feed, Feed}, {[], Entries}) ->
  {Feed, Entries};
event(endFeed, S) ->
  S.

opts() ->
  [{event_state, {[],[]}}, {event_fun, fun event/2}].

file(Filename) ->
  {ok, EventState, _Rest} = feeder:file(Filename, opts()),
  EventState.
