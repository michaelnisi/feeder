-module(feeder_example).

-export([start/0, request/1]).

start() ->
  feeder:start().

request(Url) ->
  {ok, RequestId} = feeder:request(Url),
  loop(RequestId).

loop(RequestId) ->
  receive
    {feeder, {RequestId, feed, Feed}} ->
      io:format("feed: ~p~n", [Feed]),
      loop(RequestId);
    {feeder, {RequestId, entry, Entry}} ->
      io:format("entry: ~p~n", [Entry]),
      loop(RequestId);
    {feeder, {RequestId, stream_end}} ->
      io:format("stream_end~n")
  end.
