-module(feeder_httpc_example).

-export([start/0, request/1]).

start() ->
  inets:start(),
  feeder_httpc:start_link().

request(Url) ->
  feeder_httpc:request(Url),
  loop().

loop() ->
  receive
    {feed, Feed} ->
      io:format("feed: ~p~n", [Feed]),
      loop();
    {entry, Entry} ->
      io:format("entry: ~p~n", [Entry]),
      loop();
    endFeed ->
      io:format("done~n")
  end.
