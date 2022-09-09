defmodule LoggerFormatterJsonTests.MixProject do
  use Mix.Project

  @version "0.1.0"
  @app :logger_formatter_json_tests

  def project do
    [
      app: @app,
      version: @version,
      elixir: "~> 1.13",
      start_permanent: Mix.env() == :prod,
      consolidate_protocols: Mix.env() != :test,
      deps: deps()
    ]
  end

  # Configuration for the OTP application.
  #
  # Type `mix help compile.app` for more information.
  def application do
    [
      mod: {LoggerFormatterJsonTests.Application, []},
      extra_applications:
        [:logger, :runtime_tools] ++
          extra_applications(Mix.env())
    ]
  end

  def extra_applications(:test), do: [:tools]
  def extra_applications(:dev), do: [:tools]
  def extra_applications(_), do: []

  # Specifies your project dependencies.
  #
  # Type `mix help deps` for examples and options.
  defp deps do
    [
      {:logger_formatter_json, path: "../"},
    ]
  end
end
