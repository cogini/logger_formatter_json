![test workflow](https://github.com/reachfh/logger_formatter_json/actions/workflows/test.yml/badge.svg)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](CODE_OF_CONDUCT.md)

# logger_formatter_json

A formatter for the Erlang logger application that outputs JSON.

It is a standard [formatter](https://www.erlang.org/doc/apps/kernel/logger_chapter.html#formatters)
for the high-performance `logger` application introduced in OTP 21.

It formats log messages and logger metadata as JSON, mapping
metadata names according naming conventions used by services such as
[Datadog](https://www.erlang.org/doc/man/logger_formatter.html) and
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

JSON output is mostly useful for production, as it makes it easier for tools to
parse log output. In development, a library like
[flatlog](https://github.com/ferd/flatlog) produces output that is easier for
humans to read.

In order to make all log output in consistent JSON format, including system
messages, configure the formatter as the default for all applications running
on the VM.

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

It is also possible to configure the logger just for your application and
environment.

Add a call to `:logger.add_handlers` in your application startup file, e.g.
`lib/foo/application.ex`:

```elixir
def start(_type, _args) do
    :logger.add_handlers(:foo)
```

Then add the handler to `config/prod.exs`:

```elixir
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

```elixir
config :foo, :logger, [
  {:handler, :default, :logger_std_h,
   %{
     formatter:
       {:logger_formatter_json, %{
            names: %{
                time: "date",
                level: "status",
                msg: "message"
            }
        }}
   }}
]
```

`names` is a map of keys in the metadata map to string keys in the JSON output.

The module has predefined sets keys for `datadog` and `gcp`.

```elixir
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

You can also specify a list to add your own tags to the predefined ones, e.g.
of options, e.g. `names: [datadog, %{foo: "bar"}]`.


`types` is a map which identifies the data as something the module knows how to format specially
(`level`, `system_time`, `mfa`).


`template` is a list of metadata to format. This lets you put keys in specific order to
make them easier to read in the output.

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

List elements are metadata key names, with a few special keys:

* `msg` represents the text message, if any.

If you call `logger:info("the message")`, then it would be rendered in the JSON
as `{"msg": "the message", ...}`. You can map the key `msg` to e.g. `message`
via the `names` config option.

* `all` represents all the metadata keys.
* `rest` represents all the metadata keys which have not been handled explicitly.

You can specify a group of keys as a tuple like
`{group, <name>, [<list of metadata keys>]}`, and they will be collected into a
map in the output.

For example:

```elixir
{group, source_location, [file, line, mfa]},
{group, tags, [rest]}
```

would result in:

```json
{
    "source_location": {"file:" "mymodule.ex", "line": 17, "mfa": "mymodule:thefunction/1"},
    "tags": {"foo": "bar", "biz": "baz"}
}
```

The default template is `[msg, all]`.

You can also use a tuple to specify a standard set of keys to be used:

`{keys, basic}`: `[time, level, msg]`

`{keys, trace}`: `[trace_id, span_id]`

`{keys, gcp}`:

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

```console
rebar3 steamroll
```

## Code of Conduct

This project  Contributor Covenant version 2.1. Check [CODE_OF_CONDUCT.md](/CODE_OF_CONDUCT.md) file for more information.
