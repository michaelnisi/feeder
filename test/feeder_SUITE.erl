-module(feeder_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([suite/0]).
-export([groups/0]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([atom/1]).
-export([itunes/1]).
-export([rss/1]).
-export([author/1]).
-export([measure_time/1]).

all() -> [
  {group, atom},
  {group, rss},
  {group, itunes},
  measure_time
].

suite() ->
  [{timetrap, {seconds, 60}}].

groups() -> [
  {atom, [parallel], [atom, author]},
  {rss, [parallel], [rss]},
  {itunes, [parallel], [itunes]}
].

init_per_group(_, Config) ->
  Config.

end_per_group(_, _) ->
  ok.

init_per_testcase(measure_time, _) ->
  {skip, optional};
init_per_testcase(_, Config) ->
  Config.

end_per_testcase(_, _) ->
  ok.

event({entry, Entry}, {Feed, Entries}) ->
  {Feed, [Entry|Entries]};
event({feed, Feed}, {[], Entries}) ->
  {Feed, Entries};
event(endFeed, {Feed, Entries}) ->
  {Feed, lists:reverse(Entries)}.

opts() ->
  [{event_state, {[],[]}}, {event_fun, fun event/2}].

parse(Filename) ->
  {ok, EventState, _Rest} = feeder:file(Filename, opts()),
  EventState.

name(Conf, Name) ->
  Dir = proplists:get_value(data_dir, Conf),
  [filename:join([Dir, Name])|".xml"].

-include("../src/feeder_records.hrl").

checkRecords(A, B, Fields, F) ->
  R = [{{Field, F(Field, A)}, {Field, F(Field, B)}} || Field <- Fields],
  [X = Y || {X, Y} <- R],
  ok.

feeds(A, B) ->
  Fields = record_info(fields, feed),
  F = fun(Field, Feed) -> feeder_feeds:get(Field, Feed) end,
  checkRecords(A, B, Fields, F).

enclosures(undefined, undefined) ->
  ok;
enclosures(A, B) ->
  Fields = record_info(fields, enclosure),
  F = fun(Field, Enclosure) -> feeder_enclosures:get(Field, Enclosure) end,
  checkRecords(A, B, Fields, F).

entries(A, B) ->
  enclosures(feeder_entries:get(enclosure, A), feeder_entries:get(enclosure, B)),

  Fields = record_info(fields, entry),
  F = fun(Field, Entry) -> feeder_entries:get(Field, Entry) end,
  checkRecords(A, B, Fields, F).

test(Conf, Name, {WantedFeed, WantedEntries}) ->
  Filename = name(Conf, Name),
  Found = parse(Filename),
  {FoundFeed, FoundEntries} = Found,

  %% Comparing each field individually to expose mismatches.
  feeds(WantedFeed, FoundFeed),
  [entries(A,B) || {A, B} <- lists:zip(WantedEntries, FoundEntries)],

  ok.

atom(Conf) -> test(Conf, "atom", atom:wanted()).
author(Conf) -> test(Conf, "author", author:wanted()).
rss(Conf) -> test(Conf, "rss", rss:wanted()).
itunes(Conf) -> test(Conf, "itunes", itunes:wanted()).

test_loop(_M, _F, _A, 0, List) ->
  List;
test_loop(M, F, A, N, List) ->
  {T, _Result} = timer:tc(M, F, A),
  test_loop(M, F, A, N - 1, [T|List]).

measure_time(M, F, A, N) when N > 0 ->
  L = test_loop(M, F, A, N, []),
  Length = length(L),
  Min = lists:min(L),
  Max = lists:max(L),
  Med = lists:nth(round((Length / 2)), lists:sort(L)),
  Avg = round(lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) / Length),
  ct:log(info, ?LOW_IMPORTANCE, "Range: ~.2f - ~.2f ms~n"
    "Median: ~.2f ms~n"
    "Average: ~.2f ms~n",
    [X * 0.001 || X <- [Min, Max, Med, Avg]]),
  ok.

measure_time(Conf) ->
  Filename = name(Conf, "df"),
  measure_time(feeder, file, [Filename, []], 100).
