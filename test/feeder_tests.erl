
-module(feeder_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/feeder.hrl").

%% Descriptions

rss_test_() ->
  {"should parse reference RSS",
    {setup,
     fun rss_setup/0,
     fun teardown/1,
     fun (D) ->
       [rss_feed(D),
        rss_entries(D)]
     end}}.

atom_test_() ->
  {"should parse reference Atom",
    {setup,
     fun atom_setup/0,
     fun teardown/1,
     fun (D) ->
      [atom_feed(D),
       atom_entries(D)]
     end}}.

itunes_test_() ->
  {"should parse reference iTunes",
    {setup,
     fun itunes_setup/0,
     fun teardown/1,
     fun (D) ->
       [itunes_feed(D),
        itunes_entries(D)]
     end}}.

%% Details

itunes_setup() ->
  util:file("../test/itunes.xml").

itunes_feed({Feed, _}) ->
  Expected = expected(itunes, feed),
  [?_assert(Feed =/= undefined), ?_assertMatch(Expected, Feed)].

itunes_entries({_, Entries}) ->
  [?_assert(Entries =/= undefined)].

atom_setup() ->
  util:file("../test/atom.xml").

atom_feed({Feed, _}) ->
  [?_assert(Feed =/= undefined)
  , ?_assertEqual(<<"Example Feed">>, Feed#feed.title)].

atom_entries({_, Entries}) -> [
  ?_assert(Entries =/= undefined)
, ?_assertEqual(length(Entries), 1)
].

rss_setup() ->
  util:file("../test/rss.xml").

rss_feed({Feed, _}) -> [
  ?_assert(Feed =/= undefined)
, ?_assertEqual(<<"Liftoff News">>, Feed#feed.title)
, ?_assertEqual(<<"Liftoff to Space Exploration.">>, Feed#feed.summary)
].

rss_entries({_, Entries}) ->
  Expected = expected(rss, entries),
  Actual = lists:reverse(Entries),
  [?_assert(Entries =/= undefined), ?_assertMatch(Expected, Actual)].

expected(rss, entries) ->
  [#entry{
      title = <<"Star City">>,
      link = <<"http://liftoff.msfc.nasa.gov/news/2003/news-starcity.asp">>,
      summary= <<"How do Americans get ready to work with Russians aboard the International Space Station? They take a crash course in culture, language and protocol at Russia's <a href=\"http://howe.iki.rssi.ru/GCTC/gctc_e.htm\">Star City</a>.">>,
updated= <<"Tue, 03 Jun 2003 09:39:21 GMT">>,
id= <<"http://liftoff.msfc.nasa.gov/2003/06/03.html#item573">>
},
#entry{
  summary= <<"Sky watchers in Europe, Asia, and parts of Alaska and Canada will experience a <a href=\"http://science.nasa.gov/headlines/y2003/30may_solareclipse.htm\">partial eclipse of the Sun</a> on Saturday, May 31st.">>,
updated= <<"Fri, 30 May 2003 11:06:42 GMT">>,
id= <<"http://liftoff.msfc.nasa.gov/2003/05/30.html#item572">>
},
#entry{
  title = <<"The Engine That Does More">>,
  link = <<"http://liftoff.msfc.nasa.gov/news/2003/news-VASIMR.asp">>,
  summary= <<"Before man travels to Mars, NASA hopes to design new engines that will let us fly through the Solar System more quickly.  The proposed VASIMR engine would do that.">>,
  updated= <<"Tue, 27 May 2003 08:37:32 GMT">>,
  id= <<"http://liftoff.msfc.nasa.gov/2003/05/27.html#item571">>
},
#entry{
  title = <<"Astronauts' Dirty Laundry">>,
  link = <<"http://liftoff.msfc.nasa.gov/news/2003/news-laundry.asp">>,
  summary= <<"Compared to earlier spacecraft, the International Space Station has many luxuries, but laundry facilities are not one of them.  Instead, astronauts have other options.">>,
  updated= <<"Tue, 20 May 2003 08:56:02 GMT">>,
  id= <<"http://liftoff.msfc.nasa.gov/2003/05/20.html#item570">>
}
];
expected(itunes, feed) ->
  #feed{
    title = <<"All About Everything">>,
    link = <<"http://www.example.com/podcasts/everything/index.html">>,
    summary = <<"All About Everything is a show about everything. Each week we dive into any subject known to man and talk about it as much as we can. Look for our Podcast in the iTunes Store">>
  }.

teardown(_) ->
  ok.

