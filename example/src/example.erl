-module(example).

-export([all/1]).
-export([entries/1]).
-export([feed/1]).
-export([links/1]).
-export([print_links/1]).
-export([print_titles/1]).
-export([start/0]).
-export([stop/0]).
-export([titles/1]).

start() ->
  ok = application:ensure_started(inets),
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
  [feeder_entries:get(title, Entry) || Entry <-  entries(Url)].

links(Url) ->
  [feeder_feeds:get(link, Entry) || Entry <-  entries(Url)].

print(L) ->
  lists:foreach(fun (Title) ->
    io:format("~ts~n", [Title])
    end, L),
  ok.

print_titles(Url) ->
  print(titles(Url)).

print_links(Url) ->
  print(links(Url)).
