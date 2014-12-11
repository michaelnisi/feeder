defmodule Feeder.Mixfile do
  use Mix.Project

  def project do
    [app: :feeder,
      version: "1.4.2",
      description: description,
      package: package,
      deps: deps]
  end

  defp deps do
    []
  end

  defp description do
    """
    Stream parse RSS and Atom formatted XML feeds.
    """
  end

  defp package do
    [files: ~w(src erlang.mk Makefile README.md LICENSE),
      contributors: ["Michael Nisi"],
      licenses: ["MIT"],
      links: %{"GitHub" => "https://github.com/michaelnisi/feeder"}]
  end
end
