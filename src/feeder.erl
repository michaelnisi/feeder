
%% feeder - parse RSS/Atom

-module(feeder).
-export([start/0, request/2]).

-include("../include/feeder.hrl").

start() ->
  application:start(inets).

request(Url, From) ->
  feeder_httpc:request(Url, From).
