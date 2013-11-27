
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
  Expected = feed(),
  [?_assertMatch(Expected, Feed)].

assert_entries({_, Entries}) ->
  Expected = entries(),
  [?_assertMatch(Expected, lists:reverse(Entries))].

feed() ->
  #feed{
    title = <<"All About Everything">>,
    author = <<"John Doe">>,
    link = <<"http://www.example.com/podcasts/everything/index.html">>,
    summary = <<"All About Everything is a show about everything. Each week we dive into any subject known to man and talk about it as much as we can. Look for our Podcast in the iTunes Store">>
  }.

entries() ->
  [
    #entry{
      title = <<"Shake Shake Shake Your Spices">>,
      subtitle = <<"A short primer on table spices">>,
      author = <<"John Doe">>,
      summary = <<"This week we talk about salt and pepper shakers, comparing and contrasting pour rates, construction materials, and overall aesthetics. Come and join the party!">>,
      updated = <<"Wed, 15 Jun 2005 19:00:00 GMT">>,
      id = <<"http://example.com/podcasts/archive/aae20050615.m4a">>,
      enclosure = #enclosure{
        url = <<"http://example.com/podcasts/everything/AllAboutEverythingEpisode3.m4a">>,
        length = <<"8727310">>,
        type = <<"audio/x-m4a">>
      }
    },
    #entry{
      title = <<"Socket Wrench Shootout">>,
      subtitle = <<"Comparing socket wrenches is fun!">>,
      author = <<"Jane Doe">>,
      summary = <<"This week we talk about metric vs. old english socket wrenches. Which one is better? Do you really need both? Get all of your answers here.">>,
      updated = <<"Wed, 8 Jun 2005 19:00:00 GMT">>,
      id = <<"http://example.com/podcasts/archive/aae20050608.mp3">>,
      enclosure = #enclosure{
        url = <<"http://example.com/podcasts/everything/AllAboutEverythingEpisode2.mp3">>,
        length = <<"5650889">>,
        type = <<"audio/mpeg">>
      }
    },
    #entry{
      title = <<"Red, Whine, & Blue">>,
      subtitle = <<"Red + Blue != Purple">>,
      author = <<"Various">>,
      summary = <<"This week we talk about surviving in a Red state if you are a Blue person. Or vice versa.">>,
      updated = <<"Wed, 1 Jun 2005 19:00:00 GMT">>,
      id = <<"http://example.com/podcasts/archive/aae20050601.mp3">>,
      enclosure = #enclosure{
        url = <<"http://example.com/podcasts/everything/AllAboutEverythingEpisode1.mp3">>,
        length = <<"4989537">>,
        type = <<"audio/mpeg">>
      }
    }
  ].

teardown(_) ->
  ok.

