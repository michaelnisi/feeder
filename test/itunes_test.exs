Code.require_file "common.exs", __DIR__

defmodule ITunesTest do
  use ExUnit.Case

  require Common

  setup do
    Common.parse "test/itunes.xml"
  end

  test "feed", context do
    found = context[:feed]
    wanted = %{
      author: "John Doe",
      id: nil,
      image: "http://example.com/podcasts/everything/AllAboutEverything.jpg",
      link: "http://www.example.com/podcasts/everything/index.html",
      subtitle: "A show about everything",
      summary: "All About Everything is a show about everything. Each week we dive into any subject known to man and talk about it as much as we can. Look for our Podcast in the iTunes Store",
      title: "All About Everything",
      updated: nil
    }
    assert found == wanted
  end

  test "entries", context do
    found = context[:entries]
    wanted = [
      %{author: "Various",
        enclosure: %{length: "4989537", type: "audio/mpeg",
          url: "http://example.com/podcasts/everything/AllAboutEverythingEpisode1.mp3"},
        id: "http://example.com/podcasts/archive/aae20050601.mp3",
        image: "http://example.com/podcasts/everything/AllAboutEverything/Episode3.jpg",
        link: nil, subtitle: "Red + Blue != Purple",
        summary: "This week we talk about surviving in a Red state if you are a Blue person. Or vice versa.",
        title: "Red, Whine, & Blue"},
      %{author: "Jane Doe",
        enclosure: %{length: "5650889", type: "audio/mpeg",
          url: "http://example.com/podcasts/everything/AllAboutEverythingEpisode2.mp3"},
        id: "http://example.com/podcasts/archive/aae20050608.mp3",
        image: "http://example.com/podcasts/everything/AllAboutEverything/Episode2.jpg",
        link: nil, subtitle: "Comparing socket wrenches is fun!",
        summary: "This week we talk about metric vs. old english socket wrenches. Which one is better? Do you really need both? Get all of your answers here.",
        title: "Socket Wrench Shootout"},
      %{author: "John Doe",
        enclosure: %{length: "8727310", type: "audio/x-m4a",
          url: "http://example.com/podcasts/everything/AllAboutEverythingEpisode3.m4a"},
        id: "http://example.com/podcasts/archive/aae20050615.m4a",
        image: "http://example.com/podcasts/everything/AllAboutEverything/Episode1.jpg",
        link: nil,
        subtitle: "Rückblicke, Einblicke und Ausblicke auf das netzpolitische Geschehen als wöchentlicher Podcast",
        summary: "This week we talk about salt and pepper shakers, comparing and contrasting pour rates, construction materials, and overall aesthetics. Come and join the party!",
        title: "Shake Shake Shake Your Spices"}
    ]
    assert found == wanted
  end
end
