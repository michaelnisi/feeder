-module(fedex_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  fedex_sup:start_link(),
  gen_event:add_handler(fedex_event_man, fedex_logger, []).

stop(_State) ->
  ok.
