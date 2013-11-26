
-module(rss_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/feeder.hrl").

rss_test_() ->
  {"should parse reference RSS",
    {setup,
     fun setup/0,
     fun teardown/1,
     fun (D) ->
       [assert_feed(D),
        assert_entries(D)]
     end}}.

setup() ->
  util:file("../test/rss.xml").

assert_feed({Feed, _}) ->
  Expected = feed(),
  [?_assertMatch(Expected, Feed)].

assert_entries({_, Entries}) ->
  Expected = entries(),
  Actual = lists:reverse(Entries),
  [?_assertMatch(Expected, Actual)].

feed() ->
  #feed{
    title = <<"Liftoff News">>,
    summary = <<"Liftoff to Space Exploration.">>,
    link = <<"http://liftoff.msfc.nasa.gov/">>,
    updated = <<"Tue, 10 Jun 2003 04:00:00 GMT">>
  }.

entries() ->
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
].

teardown(_) ->
  ok.

