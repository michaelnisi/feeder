
-module(example).

-export([all/1]).
-export([entries/1]).
-export([feed/1]).
-export([print_titles/1]).
-export([start/0]).
-export([stop/0]).
-export([titles/1]).

start() ->
  application:ensure_started(inets),
  application:start(?MODULE).

stop() ->
  application:stop(?MODULE).

all(Url) ->
  {ok, Pid} = supervisor:start_child(example_sup, [Url]),
  example_parse:resume(Pid).

feed(Url) ->
  {ok, Feed, _} = all(Url),
  Feed.

entries(Url) ->
  {ok, _, Entries} = all(Url),
  Entries.

titles(Url) ->
  Entries = entries(Url),
  TitleFrom = fun (Entry) -> maps:get(title, Entry) end,
  [TitleFrom(Entry) || Entry <- Entries].

print_titles(Url) ->
  Titles = titles(Url),
  lists:foreach(fun (Title) ->
    io:format("~ts~n", [Title])
    end, Titles),
  ok.
