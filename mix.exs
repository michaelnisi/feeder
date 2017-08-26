defmodule Feeder.Mixfile do
  use Mix.Project

  def project do
    [app: :feeder,
     version: "2.2.4",
     description: description(),
     package: package(),
     deps: deps(),
     name: "feeder",
     source_url: "https://github.com/michaelnisi/feeder",
     docs: [main: "readme", extras: ["README.md"]]]
  end

  def application do
    [applications: [:xmerl]]
  end

  defp deps do
    [{:ex_doc, "~> 0.16", only: :dev}]
  end

  defp description do
    """
    Stream parse RSS and Atom formatted XML feeds.
    """
  end

  defp package do
    [files: ~w(src erlang.mk Makefile README.md LICENSE),
     maintainers: ["Michael Nisi"],
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/michaelnisi/feeder"}]
  end
end
