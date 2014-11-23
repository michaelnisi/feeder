Code.require_file "common.exs", __DIR__

defmodule ITunesTest do
  use ExUnit.Case

  require Common

  setup do
    Common.parse "test/itunes.xml"
  end

  test "feed", context do
    found = context[:feed]
    wanted = {
      :feed,
      "John Doe",
      :undefined,
      "http://example.com/podcasts/everything/AllAboutEverything.jpg",
      "http://www.example.com/podcasts/everything/index.html",
      "A show about everything",
      "All About Everything is a show about everything. Each week we dive into any subject known to man and talk about it as much as we can. Look for our Podcast in the iTunes Store",
      "All About Everything",
      :undefined
    }
  assert found == wanted
  end

  test "entries", context do
    found = context[:entries]
    wanted = [
      {:entry, "Various",
        {:enclosure,
          "http://example.com/podcasts/everything/AllAboutEverythingEpisode1.mp3",
          "4989537", "audio/mpeg"},
        "http://example.com/podcasts/archive/aae20050601.mp3",
        "http://example.com/podcasts/everything/AllAboutEverything/Episode3.jpg",
        :undefined, "Red + Blue != Purple",
        "This week we talk about surviving in a Red state if you are a Blue person. Or vice versa.",
        "Red, Whine, & Blue", 1117652400},
      {:entry, "Jane Doe",
        {:enclosure,
          "http://example.com/podcasts/everything/AllAboutEverythingEpisode2.mp3",
          "5650889", "audio/mpeg"},
        "http://example.com/podcasts/archive/aae20050608.mp3",
        "http://example.com/podcasts/everything/AllAboutEverything/Episode2.jpg",
        :undefined, "Comparing socket wrenches is fun!",
        "This week we talk about metric vs. old english socket wrenches. Which one is better? Do you really need both? Get all of your answers here.",
        "Socket Wrench Shootout", 1118257200},
      {:entry, "John Doe",
        {:enclosure,
          "http://example.com/podcasts/everything/AllAboutEverythingEpisode3.m4a",
          "8727310", "audio/x-m4a"},
        "http://example.com/podcasts/archive/aae20050615.m4a",
        "http://example.com/podcasts/everything/AllAboutEverything/Episode1.jpg",
        :undefined,
        "Rückblicke, Einblicke und Ausblicke auf das netzpolitische Geschehen als wöchentlicher Podcast",
        "This week we talk about salt and pepper shakers, comparing and contrasting pour rates, construction materials, and overall aesthetics. Come and join the party!",
        "Shake Shake Shake Your Spices", 1118862000}
  ]
  assert found == wanted
  end
end
