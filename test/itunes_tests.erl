
-module(itunes_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/feeder.hrl").

itunes_test_() ->
  {"should parse reference iTunes",
    {setup,
     fun setup/0,
     fun teardown/1,
     fun (D) ->
       [assert_feed(D),
        assert_entries(D)]
     end}}.

%% Details

setup() ->
  util:file("../test/itunes.xml").

assert_feed({Feed, _}) ->
  Expected = expected(itunes, feed),
  [?_assert(Feed =/= undefined), ?_assertMatch(Expected, Feed)].

assert_entries({_, Entries}) ->
  [?_assert(Entries =/= undefined)].

expected(itunes, feed) ->
  #feed{
    title = <<"All About Everything">>,
    author = <<"John Doe">>,
    link = <<"http://www.example.com/podcasts/everything/index.html">>,
    summary = <<"All About Everything is a show about everything. Each week we dive into any subject known to man and talk about it as much as we can. Look for our Podcast in the iTunes Store">>
  }.

teardown(_) ->
  ok.

