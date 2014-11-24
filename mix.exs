defmodule Feeder.Mixfile do
  use Mix.Project

  defp erlc_options(:test), do: [{:d, :TEST}]
  defp erlc_options(_), do: []

  defp deps do [
    { :datetime,
      "~> 1.0.0",
      git: "https://github.com/lkiesow/erlang-datetime.git"
    }
  ]
  end

  defp description do
    """
    Stream parse RSS and Atom formatted XML feeds
    """
  end

  defp package do [
    files: ["src", "test", "mix.exs", "README*", "LICENSE*"],
    contributors: ["Michael Nisi"],
    licenses: ["MIT"],
    links: %{
      "GitHub" => "https://github.com/michaelnisi/feeder",
    }
  ]
  end

  def project do [
    app: :feeder,
    deps: deps,
    description: description,
    elixir: "~> 1.0.0",
    erlc_options: erlc_options(Mix.env),
    language: :erlang,
    package: package,
    version: "1.2.0"
  ]
  end

end
