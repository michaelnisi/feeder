
-module(atom_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/feeder.hrl").

atom_test_() ->
  {"should parse reference Atom",
    {setup,
     fun setup/0,
     fun teardown/1,
     fun (D) ->
      [assert_feed(D),
       assert_entries(D)]
     end}}.

setup() ->
  util:file("../test/atom.xml").

assert_feed({Feed, _}) ->
  Expected = feed(),
  [?_assert(Feed =/= undefined),
   ?_assertMatch(Expected, Feed)].

assert_entries({_, Entries}) -> [
  ?_assert(Entries =/= undefined)
, ?_assertEqual(length(Entries), 1)
].

feed() ->
  #feed{
    title = <<"Example Feed">>,
    author = <<"John Doe">>,
    link = <<"http://example.org/">>
  }.

teardown(_) ->
  ok.
