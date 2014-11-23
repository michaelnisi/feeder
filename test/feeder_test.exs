defmodule FeederTest do
  use ExUnit.Case

  test "epoch" do
    assert 0 = :feeder.epoch {{1970,1,1},{0,0,0}}
  end

  test "date" do
    catch_error :feeder.unix_time "WTF"
    assert 0 = :feeder.unix_time "WTF, 1 Jan 1970 00:00:00 GMT"
    assert 1055217600 = :feeder.unix_time "Tue, 10 Jun 2003 04:00:00 GMT"
  end

  test "trim" do
    assert <<"">> = :feeder.trim ""
    assert <<"hello">> = :feeder.trim " hello "
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
