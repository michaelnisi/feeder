
-module(atom_tests).

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
  acc:file("../test/atom.xml").

assert_feed({Feed, _}) ->
  Expected = feed(),
  [?_assertMatch(Expected, Feed)].

assert_entries({_, Entries}) ->
  Expected = entries(),
  Actual = lists:reverse(Entries),
  [?_assertMatch(Expected, Actual)].

feed() ->
  #feed{
    title = <<"Example Feed"/utf8>>,
    author = <<"John Doe"/utf8>>,
    link = <<"http://example.org/"/utf8>>,
    updated = 1400429611,
    id = <<"urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6"/utf8>>}.

entries() ->
  [#entry{
      title = <<"Atom-Powered Robots Run Amok"/utf8>>,
      link = <<"http://example.org/2003/12/13/atom03"/utf8>>,
      id = <<"urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a"/utf8>>,
      updated = 1400429611,
      summary= <<"Some text."/utf8>>}].

teardown(_) ->
  ok.
