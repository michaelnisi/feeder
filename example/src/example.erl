-module(example).

-export([start/0]).
-export([stop/0]).
-export([fetch/1]).

start() ->
  application:ensure_all_started(?MODULE),
  gen_event:start({local, example_event_man}),
  gen_event:add_handler(example_event_man, example_logger, []).

stop() ->
  application:stop(?MODULE).

fetch(Url) ->
  {ok, Pid} = supervisor:start_child(example_sup, [Url]),
  example_parse:resume(Pid).
