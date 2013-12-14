
-module(itunes_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/feeder.hrl").

itunes_test_() ->
  {"should parse reference iTunes",
    {setup,
     fun setup/0,
     fun teardown/1,
     fun (D) ->
       [feed(D),
        entries(D)]
     end}}.

%% Details

setup() ->
  util:file("../test/itunes.xml").

feed({Feed, _}) ->
  Expected = expected_feed(),
  [?_assertMatch(Expected, Feed)].

entries({_, Entries}) ->
  Expected = expected_entries(),
  [?_assertMatch(Expected, lists:reverse(Entries))].

expected_feed() ->
  #feed{
    title = <<"All About Everything">>,
    subtitle = <<"A show about everything">>,
    author = <<"John Doe">>,
    link = <<"http://www.example.com/podcasts/everything/index.html">>,
    summary = <<"All About Everything is a show about everything. Each week we dive into any subject known to man and talk about it as much as we can. Look for our Podcast in the iTunes Store">>,
    image = <<"http://example.com/podcasts/everything/AllAboutEverything.jpg">>
  }.

expected_entries() ->
  [
    #entry{
      title = <<"Shake Shake Shake Your Spices">>,
      subtitle = <<"Rückblicke, Einblicke und Ausblicke auf das netzpolitische Geschehen als wöchentlicher Podcast">>,
      author = <<"John Doe">>,
      summary = <<"This week we talk about salt and pepper shakers, comparing and contrasting pour rates, construction materials, and overall aesthetics. Come and join the party!">>,
      updated = <<"Wed, 15 Jun 2005 19:00:00 GMT">>,
      id = <<"http://example.com/podcasts/archive/aae20050615.m4a">>,
      enclosure = #enclosure{
        url = <<"http://example.com/podcasts/everything/AllAboutEverythingEpisode3.m4a">>,
        length = <<"8727310">>,
        type = <<"audio/x-m4a">>
      },
      image = <<"http://example.com/podcasts/everything/AllAboutEverything/Episode1.jpg">>
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
      },
      image = <<"http://example.com/podcasts/everything/AllAboutEverything/Episode2.jpg">>
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
      },
      image = <<"http://example.com/podcasts/everything/AllAboutEverything/Episode3.jpg">>
    }
  ].

teardown(_) ->
  ok.

