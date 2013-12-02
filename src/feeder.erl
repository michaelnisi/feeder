-module(feeder).
-export([start/0, request/1]).

-include("../include/feeder.hrl").

start() ->
  application:start(inets),
  application:load(feeder),
  application:start(feeder).

request(Url) ->
  feeder_httpc:request(Url).
