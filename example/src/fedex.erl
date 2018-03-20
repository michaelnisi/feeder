-module(fedex).

-export([start/0]).
-export([stop/0]).
-export([fetch/1]).

start() ->
  {ok, _} = application:ensure_all_started(inets),
  {ok, _} = application:ensure_all_started(ssl),
  {ok, _} = application:ensure_all_started(xmerl),
  {ok, _} = application:ensure_all_started(feeder),
  application:start(?MODULE).

stop() ->
  application:stop(?MODULE).

fetch(Url) ->
  {ok, Pid} = supervisor:start_child(fedex_parse_sup, [Url]),
  fedex_parse:resume(Pid).
