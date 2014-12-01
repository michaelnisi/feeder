
-module(itunes).
-export([wanted/0]).

wanted() -> {#{
  author => <<"John Doe">>,
  id => nil,
  image => <<"http://example.com/podcasts/everything/AllAboutEverything.jpg">>,
  language => <<"en-us">>,
  link => <<"http://www.example.com/podcasts/everything/index.html">>,
  subtitle => <<"A show about everything">>,
  summary => <<"All About Everything is a show about everything. Each week we dive into any subject known to man and talk about it as much as we can. Look for our Podcast in the iTunes Store">>,
  title => <<"All About Everything">>,
  updated => nil}, [
#{
  author => <<"John Doe">>,
  enclosure => #{
    length => <<"8727310">>,
    type => <<"audio/x-m4a">>,
    url => <<"http://example.com/podcasts/everything/AllAboutEverythingEpisode3.m4a">>},
  id => <<"http://example.com/podcasts/archive/aae20050615.m4a">>,
  image => <<"http://example.com/podcasts/everything/AllAboutEverything/Episode1.jpg">>,
  link => nil,
  duration => <<"7:04">>,
  subtitle => <<"Rückblicke, Einblicke und Ausblicke auf das netzpolitische Geschehen als wöchentlicher Podcast"/utf8>>,
  summary => <<"This week we talk about salt and pepper shakers, comparing and contrasting pour rates, construction materials, and overall aesthetics. Come and join the party!">>,
  title => <<"Shake Shake Shake Your Spices">>},
#{
  author => <<"Jane Doe">>,
  enclosure => #{
    length => <<"5650889">>,
    type => <<"audio/mpeg">>,
    url => <<"http://example.com/podcasts/everything/AllAboutEverythingEpisode2.mp3">>},
  id => <<"http://example.com/podcasts/archive/aae20050608.mp3">>,
  image => <<"http://example.com/podcasts/everything/AllAboutEverything/Episode2.jpg">>,
  link => nil,
  duration => <<"4:34">>,
  subtitle => <<"Comparing socket wrenches is fun!">>,
  summary => <<"This week we talk about metric vs. old english socket wrenches. Which one is better? Do you really need both? Get all of your answers here.">>,
  title => <<"Socket Wrench Shootout">>},
#{
  author => <<"Various">>,
  enclosure => #{
    length => <<"4989537">>,
    type => <<"audio/mpeg">>,
    url => <<"http://example.com/podcasts/everything/AllAboutEverythingEpisode1.mp3">>},
  id => <<"http://example.com/podcasts/archive/aae20050601.mp3">>,
  image => <<"http://example.com/podcasts/everything/AllAboutEverything/Episode3.jpg">>,
  link => nil,
  duration => <<"3:59">>,
  subtitle => <<"Red + Blue != Purple">>,
  summary => <<"This week we talk about surviving in a Red state if you are a Blue person. Or vice versa.">>,
  title => <<"Red, Whine, & Blue">>}
]}.
