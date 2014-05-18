
-module(feeder_tests).

-include("../include/feeder.hrl").

date_test_() -> [
  ?_assertError({badmatch, ["WTF"]}, feeder:unix_time("WTF")),
  ?_assertEqual(0,
    feeder:unix_time("WTF, 1 Jan 1970 00:00:00 GMT")),
  ?_assertEqual(1055217600,
    feeder:unix_time("Tue, 10 Jun 2003 04:00:00 GMT"))
].

trim_test_() -> [
  ?_assertMatch(<<"">>, feeder:trim("")),
  ?_assertMatch(<<"hello">>, feeder:trim(" hello "))
].

qname_test_() -> [
  ?_assertMatch(feed, feeder:qname({"", "channel"})),
  ?_assertMatch(feed, feeder:qname({"", "feed"})),
  ?_assertMatch(entry, feeder:qname({"", "item"})),
  ?_assertMatch(entry, feeder:qname({"", "entry"})),
  ?_assertMatch(title, feeder:qname({"", "title"})),
  ?_assertMatch(subtitle, feeder:qname({"", "subtitle"})),
  ?_assertMatch(summary, feeder:qname({"", "description"})),
  ?_assertMatch(summary, feeder:qname({"", "summary"})),
  ?_assertMatch(link, feeder:qname({"", "link"})),
  ?_assertMatch(updated, feeder:qname({"", "pubDate"})),
  ?_assertMatch(updated, feeder:qname({"", "updated"})),
  ?_assertMatch(id, feeder:qname({"", "guid"})),
  ?_assertMatch(id, feeder:qname({"", "id"})),
  ?_assertMatch(name, feeder:qname({"", "name"})),
  ?_assertMatch(author, feeder:qname({"", "author"})),
  ?_assertMatch(enclosure, feeder:qname({"", "enclosure"})),
  ?_assertMatch(undefined, feeder:qname({"", "anything_else"}))
].
