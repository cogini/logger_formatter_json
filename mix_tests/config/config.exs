# This file is responsible for configuring your application
# and its dependencies with the aid of the Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
import Config

config :logger,
  level: :info,
  utc_log: true

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id, :trace_id, :span_id]

config :logger_formatter_json_tests, :logger, [
  {:handler, :default, :logger_std_h,
   %{
     formatter:
       {:logger_formatter_json,
        %{
          names: :datadog
        }}
   }}
]

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{config_env()}.exs"
