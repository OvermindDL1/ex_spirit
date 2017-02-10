defmodule ExSpirit.Mixfile do
  use Mix.Project

  def project do
    [app: :ex_spirit,
     version: "0.1.2",
     elixir: "~> 1.4",
     description: description(),
     package: package(),
     docs: [
        #logo: "path/to/logo.png",
        extras: ["README.md"],
        main: "readme",
        ],
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps()]
  end

  def application do
    [extra_applications: []]
  end

  def description do
    """
    Spirit-style PEG-like parsing library for Elixir.
    """
  end

  def package do
    [
      licenses: ["MIT"],
      name: :ex_spirit,
      maintainers: ["OvermindDL1"],
      links: %{"Github" => "https://github.com/OvermindDL1/ex_spirit"}
    ]
  end

  defp deps do
    [
      {:ex_doc, "~> 0.14.5", only: [:dev]},
    ]
  end
end
