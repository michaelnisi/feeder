
-module(feeder_parser_tests).

-include("../include/feeder.hrl").

trim_test_() -> [
  ?_assertMatch(<<"">>, feeder_parser:trim("")),
  ?_assertMatch(<<"hello">>, feeder_parser:trim(" hello "))
].

qname_test_() -> [
  ?_assertMatch(feed, feeder_parser:qname({"", "channel"})),
  ?_assertMatch(feed, feeder_parser:qname({"", "feed"})),
  ?_assertMatch(entry, feeder_parser:qname({"", "item"})),
  ?_assertMatch(entry, feeder_parser:qname({"", "entry"})),
  ?_assertMatch(title, feeder_parser:qname({"", "title"})),
  ?_assertMatch(subtitle, feeder_parser:qname({"", "subtitle"})),
  ?_assertMatch(summary, feeder_parser:qname({"", "description"})),
  ?_assertMatch(summary, feeder_parser:qname({"", "summary"})),
  ?_assertMatch(link, feeder_parser:qname({"", "link"})),
  ?_assertMatch(updated, feeder_parser:qname({"", "pubDate"})),
  ?_assertMatch(updated, feeder_parser:qname({"", "updated"})),
  ?_assertMatch(id, feeder_parser:qname({"", "guid"})),
  ?_assertMatch(id, feeder_parser:qname({"", "id"})),
  ?_assertMatch(name, feeder_parser:qname({"", "name"})),
  ?_assertMatch(author, feeder_parser:qname({"", "author"})),
  ?_assertMatch(enclosure, feeder_parser:qname({"", "enclosure"})),
  ?_assertMatch(undefined, feeder_parser:qname({"", "anything_else"}))
].
