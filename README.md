# logger_formatter_json

A formatter for the Erlang logger application that outputs JSON.

The logger application was introduced in OTP 21.
https://www.erlang.org/doc/apps/kernel/logger_chapter.html#formatters

## Usage

For Erlang, configure the default handler in the `sys.config` file for the
release:

```erlang
[
 {kernel, [
    {logger, [
        {handler, default, logger_std_h,
         #{formatter => {logger_formatter_json, #{}}}
        }
    ]},
    {logger_level, info}
 ]}
].
```

For Elixir, configre the default logger in `config/config.exs` file:

```elixir
if System.get_env("RELEASE_MODE") do
  config :kernel, :logger, [
    {:handler, :default, :logger_std_h,
     %{
       formatter: {:logger_formatter_json, %{}}
     }}
  ]
end
```

with options:

```elixir
if System.get_env("RELEASE_MODE") do
  config :kernel, :logger, [
    {:handler, :default, :logger_std_h,
     %{
       formatter:
         {:logger_formatter_json,
          %{
            names: :datadog
            template: [
              :msg,
              :time,
              :level,
              :file,
              :line,
              :mfa,
              :pid,
              :trace_id,
              :span_id
            ]
          }}
     }}
  ]
end
```

Add config for your application:

```elixir
config :foo, :logger, [
  {:handler, :default, :logger_std_h,
   %{
     formatter:
       {:logger_formatter_json,
        %{
          names: :datadog
          template: [
            :msg,
            :time,
            :level,
            :file,
            :line,
            :mfa,
            :pid,
            :trace_id,
            :span_id
          ]
        }}
   }}
]
```

Add a call to `:logger.add_handlers` in your application startup file, e.g.
`lib/foo/application.ex`:

```elixir
def start(_type, _args) do
    :logger.add_handlers(:foo)
```

## Build

```console
rebar3 compile
```

## Test

```console
rebar3 ct
```

## Format

On Erlang 25, enable all features to run the code formatter.

```console
ERL_FLAGS="-enable-feature all" rebar3 format
```
