Code.require_file "common.exs", __DIR__

defmodule RSSTest do
  use ExUnit.Case

  require Common

  setup do
    Common.parse "test/rss.xml"
  end

  test "feed", context do
    found = context[:feed]
    wanted = %{
      author: nil,
      id: nil,
      image: nil,
      link: "http://liftoff.msfc.nasa.gov/",
      subtitle: nil,
      summary: "Liftoff to Space Exploration.",
      title: "Liftoff News",
      updated: "Tue, 10 Jun 2003 04:00:00 GMT"
    }
    assert found == wanted
  end

  test "entries", context do
    found = context[:entries]
    wanted = [
      %{author: nil, enclosure: nil,
        id: "http://liftoff.msfc.nasa.gov/2003/05/20.html#item570",
        image: nil,
        link: "http://liftoff.msfc.nasa.gov/news/2003/news-laundry.asp",
        subtitle: nil,
        summary: "Compared to earlier spacecraft, the International Space Station has many luxuries, but laundry facilities are not one of them.  Instead, astronauts have other options.",
        title: "Astronauts' Dirty Laundry"},
      %{author: nil, enclosure: nil,
        id: "http://liftoff.msfc.nasa.gov/2003/05/27.html#item571",
        image: nil,
        link: "http://liftoff.msfc.nasa.gov/news/2003/news-VASIMR.asp",
        subtitle: nil,
        summary: "Before man travels to Mars, NASA hopes to design new engines that will let us fly through the Solar System more quickly.  The proposed VASIMR engine would do that.",
        title: "The Engine That Does More"},
      %{author: nil, enclosure: nil,
        id: "http://liftoff.msfc.nasa.gov/2003/05/30.html#item572",
        image: nil, link: nil, subtitle: nil,
        summary: "Sky watchers in Europe, Asia, and parts of Alaska and Canada will experience a <a href=\"http://science.nasa.gov/headlines/y2003/30may_solareclipse.htm\">partial eclipse of the Sun</a> on Saturday, May 31st.",
        title: nil},
      %{author: nil, enclosure: nil,
        id: "http://liftoff.msfc.nasa.gov/2003/06/03.html#item573",
        image: nil,
        link: "http://liftoff.msfc.nasa.gov/news/2003/news-starcity.asp",
        subtitle: nil,
        summary: "How do Americans get ready to work with Russians aboard the International Space Station? They take a crash course in culture, language and protocol at Russia's <a href=\"http://howe.iki.rssi.ru/GCTC/gctc_e.htm\">Star City</a>.",
        title: "Star City"}
    ]
  assert found == wanted
  end
end
