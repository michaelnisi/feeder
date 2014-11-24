
# common.exs - common stuff for testing

defmodule Common do
  defp event({:entry, entry}, {feed, entries}) do
    {feed, [entry|entries]}
  end
  defp event({:feed, feed}, {[], entries}) do
    {feed, entries}
  end
  defp event(:endFeed, acc) do
    acc
  end

  # Parse file and return accumulated event data.
  def parse(filename) do
    opts = [{:event_state, {[],[]}}, {:event_fun, &event/2}]
    {:ok, state, _rest} = :feeder.file filename, opts
    {feed, entries} = state
    {:ok, [feed: feed, entries: entries]}
  end
end
