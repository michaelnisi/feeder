-module(fedex).

-export([start/0]).
-export([stop/0]).
-export([fetch/1]).

start() ->
  application:ensure_all_started(?MODULE),
  gen_event:add_handler(fedex_event_man, fedex_logger, []).

stop() ->
  application:stop(?MODULE).

fetch(Url) ->
  {ok, Pid} = supervisor:start_child(fedex_parse_sup, [Url]),
  fedex_parse:resume(Pid).
