
-module(itunes_tests).

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
  acc:file("../test/itunes.xml").

feed({Feed, _}) ->
  Wanted = wanted_feed(),
  [?_assertMatch(Wanted, Feed)].

entries({_, Entries}) ->
  Wanted = wanted_entries(),
  Found = lists:reverse(Entries),
  [?_assertMatch(Wanted, Found)].

wanted_feed() ->
  #feed{
    title = <<"All About Everything"/utf8>>,
    subtitle = <<"A show about everything"/utf8>>,
    author = <<"John Doe"/utf8>>,
    link = <<"http://www.example.com/podcasts/everything/index.html"/utf8>>,
    summary = <<"All About Everything is a show about everything. Each week we dive into any subject known to man and talk about it as much as we can. Look for our Podcast in the iTunes Store"/utf8>>,
    image = <<"http://example.com/podcasts/everything/AllAboutEverything.jpg"/utf8>>
  }.

wanted_entries() ->
  [
    #entry{
      title = <<"Shake Shake Shake Your Spices"/utf8>>,
      subtitle = <<"Rückblicke, Einblicke und Ausblicke auf das netzpolitische Geschehen als wöchentlicher Podcast"/utf8>>,
      author = <<"John Doe"/utf8>>,
      summary = <<"This week we talk about salt and pepper shakers, comparing and contrasting pour rates, construction materials, and overall aesthetics. Come and join the party!"/utf8>>,
      updated = 1118862000,
      id = <<"http://example.com/podcasts/archive/aae20050615.m4a"/utf8>>,
      enclosure = #enclosure{
        url = <<"http://example.com/podcasts/everything/AllAboutEverythingEpisode3.m4a"/utf8>>,
        length = <<"8727310"/utf8>>,
        type = <<"audio/x-m4a"/utf8>>
      },
      image = <<"http://example.com/podcasts/everything/AllAboutEverything/Episode1.jpg"/utf8>>
    },
    #entry{
      title = <<"Socket Wrench Shootout"/utf8>>,
      subtitle = <<"Comparing socket wrenches is fun!"/utf8>>,
      author = <<"Jane Doe"/utf8>>,
      summary = <<"This week we talk about metric vs. old english socket wrenches. Which one is better? Do you really need both? Get all of your answers here."/utf8>>,
      updated = 1118257200,
      id = <<"http://example.com/podcasts/archive/aae20050608.mp3"/utf8>>,
      enclosure = #enclosure{
        url = <<"http://example.com/podcasts/everything/AllAboutEverythingEpisode2.mp3"/utf8>>,
        length = <<"5650889"/utf8>>,
        type = <<"audio/mpeg"/utf8>>
      },
      image = <<"http://example.com/podcasts/everything/AllAboutEverything/Episode2.jpg"/utf8>>
    },
    #entry{
      title = <<"Red, Whine, & Blue"/utf8>>,
      subtitle = <<"Red + Blue != Purple"/utf8>>,
      author = <<"Various"/utf8>>,
      summary = <<"This week we talk about surviving in a Red state if you are a Blue person. Or vice versa."/utf8>>,
      updated = 1117652400,
      id = <<"http://example.com/podcasts/archive/aae20050601.mp3"/utf8>>,
      enclosure = #enclosure{
        url = <<"http://example.com/podcasts/everything/AllAboutEverythingEpisode1.mp3"/utf8>>,
        length = <<"4989537"/utf8>>,
        type = <<"audio/mpeg"/utf8>>
      },
      image = <<"http://example.com/podcasts/everything/AllAboutEverything/Episode3.jpg"/utf8>>
    }
  ].

teardown(_) ->
  ok.

