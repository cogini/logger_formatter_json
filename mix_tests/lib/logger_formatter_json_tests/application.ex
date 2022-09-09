defmodule LoggerFormatterJsonTests.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    :logger.add_handlers(:logger_formatter_json_tests)

    children = []

    opts = [strategy: :one_for_one, name: LoggerFormatterJsonTests.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
