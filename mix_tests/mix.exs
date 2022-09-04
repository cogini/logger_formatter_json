defmodule LoggerFormatterJsonTests.MixProject do
  use Mix.Project

  @version "0.1.0"

  def project do
    [
      app: :logger_formatter_json_tests,
      version: @version,
      elixir: "~> 1.13",
      start_permanent: Mix.env() == :prod,
      consolidate_protocols: Mix.env() != :test,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: []
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:logger_formatter_json, path: "../"},
    ]
  end
end
