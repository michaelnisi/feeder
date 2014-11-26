Code.require_file "common.exs", __DIR__

defmodule AtomTest do
  use ExUnit.Case

  require Common

  setup do
    Common.parse "test/atom.xml"
  end

  test "feed", context do
    found = context[:feed]
    wanted = %{
      author: "John Doe",
      id: "urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6",
      image: nil,
      link: "http://example.org/",
      subtitle: nil,
      summary: nil,
      title: "Example Feed",
      updated: "Sun, 18 May 2014 16:13:31 GMT"
    }
    assert found == wanted
  end

  test "entries", context do
    found = context[:entries]
    wanted = [
      %{author: nil, enclosure: nil,
        id: "urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a",
        image: nil, link: "http://example.org/2003/12/13/atom03",
        subtitle: nil, summary: "Some text.",
        title: "Atom-Powered Robots Run Amok"}
    ]
    assert found == wanted
  end
end
