![test workflow](https://github.com/reachfh/logger_formatter_json/actions/workflows/test.yml/badge.svg)

# logger_formatter_json

A formatter for the Erlang logger application that outputs JSON.

It is a standard [formatter](https://www.erlang.org/doc/apps/kernel/logger_chapter.html#formatters)
for the high-performance `logger` application introduced in OTP 21.

It formats log messages and logger metadata as JSON, mapping
metadata names according naming conventions used by services such as
[Datadog](https://www.erlang.org/doc/man/logger_formatter.html) or
[Google Cloud](https://cloud.google.com/logging/docs/reference/v2/rest/v2/LogEntry#LogSeverity)

It is written in Erlang with no dependencies except for the JSON library
[thoas](https://github.com/lpil/thoas). It can be used by pure Erlang projects,
as well as other BEAM languages such as Elixir.

## Installation

## Erlang

Add `logger_formatter_json` to the list of dependencies in `rebar.config`:

```erlang
{deps, [logger_formatter_json]}.
```

## Elixir

Add `logger_formatter_json` to the list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:logger_formatter_json, "~> 0.7"},
  ]
end
```

## Usage

The formatter is normally configured as part of the production release, making
it the default for all applications running on the VM.

### Erlang

Configure the kernel default handler in the `sys.config` file for the release:

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

## Elixir

The Elixir logging system is brought up after the kernel logger, so setting it
as part of the release runtime is the most reliable way of getting everything
to use JSON output.

Configure the kernel default logger in `config/config.exs`:

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

The check for the `RELEASE_MODE` environment variable makes the code
only run when building a release.

Set options for the Elixir logging system in `config/prod.exs`:

```elixir
config :logger,
  level: :info,
  utc_log: true

config :logger, :console,
  metadata: [:time, :level, :file, :line, :mfa, :pid, :request_id, :trace_id, :span_id]
```

It is also possible to configure the logger just for your application
and environment.

Add a call to `:logger.add_handlers` in your application startup file, e.g.
`lib/foo/application.ex`:

```elixir
def start(_type, _args) do
    :logger.add_handlers(:foo)
```

Then add the handler to `config/prod.exs`:

```elxixir
config :foo, :logger, [
  {:handler, :default, :logger_std_h,
   %{
     formatter:
       {:logger_formatter_json, %{}}
   }}
]
```

## Configuration

The formatter accepts a map of options, e.g.:

```elxixir
config :foo, :logger, [
  {:handler, :default, :logger_std_h,
   %{
     formatter:
       {:logger_formatter_json, %{
            names: :datadog
        }}
   }}
]
```

`names`: Mapping from the key in the metadata map to a string key used in the JSON output.

The module has predefined keys for `datadog` and `gcp`. You can also specify a
list of options, e.g. `[datadog, %{foo: "bar"}]`.

`types`: Mapping from the key in the metadata map to a special type that the
module knows how to format (`level`, `system_time`, `mfa`).

`template`: List of metadata to format.

For example:

```elixir
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
```

List elements are mostly keys in the metatada map.

A few keys are special:

* `msg` represents the text message, if any.

If you call `logger:info("the message")`, then it would be rendered in the JSON
as `{"msg": "the message", ...}`. You can map the key `msg` to e.g. `message`
via the `names` config option.

* `all` represents all the metadata keys.
* `rest` represents all the metadata keys which have not been handled explicitly.

You can specify a group of keys as a tuple with
`{group, `<key>`, [<list of metadata keys>]}, and they will be collected into a
map in the output.

For example:

```elixir
{group, source_location, [file, line, mfa]},
{group, tags, [rest]}
```

The default template is `[msg, all]`.

You can also use a tuple to specify a set of keys to be used:

`{keys, basic}`: `[time, level, msg]`

`{keys, gcp}`: `
```erlang
[
    msg,
    time,
    level,
    trace_id,
    span_id,
    {group, source_location, [file, line, mfa]},
    {group, tags, [rest]}
]
```

`{keys, trace}`: `[trace_id, span_id]`


You can specify multple templates, so you can add your own, e.g.
`[{keys, basic}, request_id, trace_id, span_id]`.


## Build

```console
rebar3 compile
```

## Test

```console
rebar3 ct
```

There are also tests written in Elixir. Change to the `mix_tests` directory and run:

```console
mix deps.get
mix test
```

## Format code

To run the code formatter on OTP 25:

```console
ERL_FLAGS="-enable-feature all" rebar3 format
```
