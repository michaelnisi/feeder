-module(terminal_logger).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, terminate/2]).

init(_Args) ->
  {ok, []}.

handle_event({entry, Entry}, State) ->
  Title = feeder_entries:get(title, Entry),
  io:format("~ts~n", [Title]),
  {ok, State};
handle_event({feed, _}, State) ->
  {ok, State}.

handle_call(_Request, _State) ->
  {remove_handler, error}.

terminate(_Args, _State) ->
  ok.
