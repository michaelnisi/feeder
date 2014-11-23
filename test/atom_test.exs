Code.require_file "common.exs", __DIR__

defmodule AtomTest do
  use ExUnit.Case

  require Common

  setup do
    Common.parse "test/atom.xml"
  end

  test "feed", context do
    found = context[:feed]
    wanted = {
      :feed,
      "John Doe",
      "urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6",
      :undefined,
      "http://example.org/",
      :undefined,
      :undefined,
      "Example Feed",
      1400429611
    }
    assert found == wanted
  end

  test "entries", context do
    found = context[:entries]
    wanted = [{
      :entry,
      :undefined,
      :undefined,
      "urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a",
      :undefined,
      "http://example.org/2003/12/13/atom03",
      :undefined,
      "Some text.",
      "Atom-Powered Robots Run Amok",
      1400429611
    }]
    assert found == wanted
  end
end
