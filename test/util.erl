
-module(util).
-export([stream/1, file/1, is/2]).
-include_lib("eunit/include/eunit.hrl").

is([],[]) ->
  ok;
is([AH|AT], [BH|BT]) ->
  ?assertMatch(AH, BH),
  is(AT, BT).

file(Filename) ->
  {ok, _EventState, _Rest} = feeder:file(Filename, opts()),
  loop({{}, []}).

stream(Chunk) ->
  feeder:stream(Chunk, opts()),
  loop({{}, []}).

opts() ->
  [{event_state, {}}, {event_fun, fun event/2}].

loop({_Feed, Entries}) ->
  receive
    {entry, Entry} ->
      loop({_Feed, [Entry|Entries]});
    {feed, Feed} ->
      loop({Feed, Entries});
    endFeed ->
      {_Feed, Entries}
  end.

event({entry, Entry}, S) ->
  self() ! {entry, Entry},
  S;
event({feed, Feed}, S) ->
  self() ! {feed, Feed},
  S;
event(endFeed, S) ->
self() ! endFeed,
  S;
event(_, S) ->
  S.
