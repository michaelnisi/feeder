
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
  [?_assertMatch(Expected, Feed)].

assert_entries({_, Entries}) ->
  Expected = entries(),
  Actual = lists:reverse(Entries),
  [?_assertMatch(Expected, Actual)].

feed() ->
  #feed{
    title = <<"Example Feed">>,
    author = <<"John Doe">>,
    link = <<"http://example.org/">>,
    updated = <<"2003-12-13T18:30:02Z">>,
    id = <<"urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6">>}.

entries() ->
  [#entry{
      title = <<"Atom-Powered Robots Run Amok">>,
      link = <<"http://example.org/2003/12/13/atom03">>,
      id = <<"urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a">>,
      updated = <<"2003-12-13T18:30:02Z">>,
      summary= <<"Some text.">>}].

teardown(_) ->
  ok.
