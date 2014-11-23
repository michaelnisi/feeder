defmodule Feeder.Mixfile do
  use Mix.Project

  defp erlc_opts(:test), do: [
    {:d, :TEST}
  ]
  defp erlc_opts(_), do: []

  defp deps do
    [{ :datetime, "~> 1.0.0", git: "https://github.com/lkiesow/erlang-datetime.git"}]
  end

  def project do [
    app: :feeder,
    deps: deps,
    elixir: "~> 1.0.0",
    erlc_options: erlc_opts(Mix.env),
    language: :erlang,
    version: "1.1.0"
  ]
  end

end
