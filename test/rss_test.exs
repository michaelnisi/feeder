Code.require_file "common.exs", __DIR__

defmodule RSSTest do
  use ExUnit.Case

  require Common

  setup do
    Common.parse "test/rss.xml"
  end

  test "feed", context do
    found = context[:feed]
    wanted = {
      :feed,
      :undefined,
      :undefined,
      :undefined,
      "http://liftoff.msfc.nasa.gov/",
      :undefined,
      "Liftoff to Space Exploration.",
      "Liftoff News",
      1055217600
    }
    assert found == wanted
  end

  test "entries", context do
    found = context[:entries]
    wanted = [
      {:entry, :undefined, :undefined,
        "http://liftoff.msfc.nasa.gov/2003/05/20.html#item570",
        :undefined,
        "http://liftoff.msfc.nasa.gov/news/2003/news-laundry.asp",
        :undefined,
        "Compared to earlier spacecraft, the International Space Station has many luxuries, but laundry facilities are not one of them.  Instead, astronauts have other options.",
        "Astronauts' Dirty Laundry", 1053420962},
      {:entry, :undefined, :undefined,
        "http://liftoff.msfc.nasa.gov/2003/05/27.html#item571",
        :undefined,
        "http://liftoff.msfc.nasa.gov/news/2003/news-VASIMR.asp",
        :undefined,
        "Before man travels to Mars, NASA hopes to design new engines that will let us fly through the Solar System more quickly.  The proposed VASIMR engine would do that.",
        "The Engine That Does More", 1054024652},
      {:entry, :undefined, :undefined,
        "http://liftoff.msfc.nasa.gov/2003/05/30.html#item572",
        :undefined, :undefined, :undefined,
        "Sky watchers in Europe, Asia, and parts of Alaska and Canada will experience a <a href=\"http://science.nasa.gov/headlines/y2003/30may_solareclipse.htm\">partial eclipse of the Sun</a> on Saturday, May 31st.",
        :undefined, 1054292802},
      {:entry, :undefined, :undefined,
        "http://liftoff.msfc.nasa.gov/2003/06/03.html#item573",
        :undefined,
        "http://liftoff.msfc.nasa.gov/news/2003/news-starcity.asp",
        :undefined,
        "How do Americans get ready to work with Russians aboard the International Space Station? They take a crash course in culture, language and protocol at Russia's <a href=\"http://howe.iki.rssi.ru/GCTC/gctc_e.htm\">Star City</a>.",
        "Star City", 1054633161}
    ]
    assert found == wanted
  end
end
