#! /usr/bin/env escript

-mode(compile).
-include("../include/feeder.hrl").

main([]) ->
  code:add_pathz("test"),
  code:add_pathz("ebin"),
  etap:plan(4),
  file(),
  stream(),
  etap:end_tests().

file() ->
  Entries = common:file("test/rss2sample.xml"),
  etap:is(4, length(Entries), "should be 4"),
  ok.

stream() ->
  Entries = common:stream("<channel><item><author>John Doe</author><title>Nice Title</title></item><item><title>Nice Title</title><author>John Doe</author></item></channel>"),
  Entry = #entry{author = <<"John Doe">>, title = <<"Nice Title">>},
  [etap:is(E, Entry,  "should be the same") || E <- Entries],
  etap:is(2, length(Entries), "should be 2"),
  ok.
