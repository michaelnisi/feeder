defmodule FeederTest do
  use ExUnit.Case

  test "trim" do
    assert "" = :feeder.trim ''
    assert "hello" = :feeder.trim ' hello '
  end

  def qname([h|t]) do
    wanted = elem h, 1
    found = :feeder.qname elem h, 0
    assert wanted == found
    qname t
  end
  def qname([]) do
    :ok
  end

  test "qname" do
    :ok = qname [
      {{'', 'channel'}, :feed},
      {{'', 'feed'}, :feed},
      {{'', 'item'}, :entry},
      {{'', 'entry'}, :entry},
      {{'', 'title'}, :title},
      {{'', 'subtitle'}, :subtitle},
      {{'', 'description'}, :summary},
      {{'', 'summary'}, :summary},
      {{'', 'link'}, :link},
      {{'', 'updated'}, :updated},
      {{'', 'pubDate'}, :updated},
      {{'', 'id'}, :id},
      {{'', 'guid'}, :id},
      {{'', 'name'}, :name},
      {{'', 'author'}, :author},
      {{'', 'enclosure'}, :enclosure},
      {{'', 'anything_else'}, :undefined}
    ]
  end

end
