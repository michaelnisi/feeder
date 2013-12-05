
%% feeder - to test in shell

-module(feeder).
-export([start/0, file/2, stream/2, url/2]).

-include("../include/feeder.hrl").

start() ->
  application:start(inets).

file(Filename, Opts) ->
  feeder_parser:file(Filename, Opts).

stream(Xml, Opts) ->
  feeder_parser:stream(Xml, Opts).

url(Url, From) ->
  feeder_httpc:request(Url, From).
