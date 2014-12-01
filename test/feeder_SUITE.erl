
-module(feeder_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([suite/0]).
-export([groups/0]).
-export([init_per_group/2]).
-export([end_per_group/2]).

-export([trim/1]).
-export([qname/1]).
-export([atom/1]).
-export([itunes/1]).
-export([rss/1]).

all() ->
	[{group, internal}, {group, parse}].

suite() ->
	[{timetrap, {seconds, 60}}].

groups() -> [
  {internal, [parallel], [trim, qname]},
  {parse, [parallel], [atom, rss, itunes]}
].

init_per_group(_, Config) ->
  Config.

end_per_group(_, _) ->
  ok.

trim(_) ->
  <<"">> = feeder:trim(""),
  <<"hello">> = feeder:trim(" hello "),
  ok.

q([{Wanted, Names}|T]) ->
  F = fun (Name) -> Wanted = feeder:qname({"", Name}) end,
  lists:map(F, Names),
  q(T);
q([]) ->
  ok.

qname(_) -> q([
  {author, ["author"]},
  {enclosure, ["enclosure"]},
  {entry, ["entry", "item"]},
  {feed, ["feed", "channel"]},
  {id, ["id", "guid"]},
  {image, ["image"]},
  {link, ["link"]},
  {subtitle, ["subtitle"]},
  {summary, ["summary", "description"]},
  {title, ["title"]},
  {undefined, ["wtf", "", "!"]},
  {updated, ["updated", "pubDate"]}
]).

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

test(Conf, Name, Wanted) ->
  Dir = proplists:get_value(data_dir, Conf),
  Filename= [filename:join([Dir, Name])|".xml"],
  Found = parse(Filename),
  Wanted = Found,
  ok.

atom(Conf) -> test(Conf, "atom", atom:wanted()).
rss(Conf) -> test(Conf, "rss", rss:wanted()).
itunes(Conf) -> test(Conf, "itunes", itunes:wanted()).
