![test workflow](https://github.com/reachfh/logger_formatter_json/actions/workflows/test.yml/badge.svg)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](CODE_OF_CONDUCT.md)

# logger_formatter_json

This is a formatter for the Erlang logger application that outputs JSON.

It implements the [formatter](https://www.erlang.org/doc/apps/kernel/logger_chapter.html#formatters)
API for the high-performance `logger` application introduced in OTP 21.

It formats log messages and logger metadata as JSON, supporting
naming conventions from services such as [Datadog](https://www.erlang.org/doc/man/logger_formatter.html) and
[Google Cloud](https://cloud.google.com/logging/docs/reference/v2/rest/v2/LogEntry#LogSeverity)

It is written in Erlang with no dependencies except for the Erlang JSON library
[thoas](https://github.com/lpil/thoas). It can be used by pure Erlang projects
as well as other BEAM languages such as Elixir.

## Installation

Erlang:

Add `logger_formatter_json` to the list of dependencies in `rebar.config`:

```erlang
{deps, [logger_formatter_json]}.
```

Elixir:

Add `logger_formatter_json` to the list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:logger_formatter_json, "~> 0.7"},
  ]
end
```

## Configuration

JSON output is useful for production, as it allows you parse and query
log records to, e.g., match on a particular user. It can be excessively verbose
for development, however, so we stick with the normal Elixir logger for development.
For Erlang, [flatlog](https://github.com/ferd/flatlog) is a similar user-friendly library.

In order to make all log output in consistent JSON format, including system
messages, we configure the formatter as the default for all applications
running on the VM.

Erlang:

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

Elixir:

As of Elixir 15, we can override the formatter for the default log handler
easily. In `config/prod.exs` configure `:default_handler`:

```elixir
config :logger, :default_handler,
  formatter: {:logger_formatter_json, %{}}
```

or, with options (see below):

```elixir
config :logger, :default_handler,
  formatter: {
    :logger_formatter_json,
    %{
      template: [
        :msg,
        :time,
        :level,
        :file,
        :line,
        # :mfa,
        :pid,
        :request_id,
        :trace_id,
        :span_id
      ]
    }
  }
```

In older Elixir versions, it was tricky to configure the default handler from
Elixir. Instead, we would reconfigure the default handler in the app startup.

In `config/prod.exs` or `config/runtime.exs`, define the formatter config:

```elixir
config :foo, :logger_formatter_config, {:logger_formatter_json, %{}}
```

or, with options (see below):

```elixir
config :foo, :logger_formatter_config, {:logger_formatter_json,
 %{
   template: [
     :msg,
     :time,
     :level,
     :file,
     :line,
     # :mfa,
     :pid,
     :request_id,
     :trace_id,
     :span_id
   ]
 }}
```

Next, in in your application startup file, e.g., `lib/foo/application.ex`, add a
call to reconfigure the logger:

```elixir
def start(_type, _args) do
  logger_formatter_config = Application.get_env(:foo, :logger_formatter_config)

  if logger_formatter_config do
    :logger.update_handler_config(:default, :formatter, logger_formatter_config)
  end
```

If you want all the messages from the initial startup in JSON as well, you have to
configure the logger as a VM arg for the release.

In `rel/vm.args.eex`, set up the logger:

```erlang
-kernel logger '[{handler, default, logger_std_h, #{formatter => {logger_formatter_json, #{}}}}]'
```
or, with options:

```erlang
-kernel logger '[{handler, default, logger_std_h, #{formatter => {logger_formatter_json, #{template => [msg, time, level, file, line, mfa, pid, trace_id, span_id]}}}}]'
```

There used to be a way of doing this in Elixir, but it seems to have stopped
working. In `config/prod.exs` or `config/runtime.exs`, define the formatter
config:

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

The check for the `RELEASE_MODE` environment variable makes the code only run
when building a release.


## Usage

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

The module has predefined sets of keys for `datadog` and `gcp`.

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

`types` is a map which identifies keys with a special format that the module understands
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

This would result in a log message like:

```json
{
    ...
    "source_location": {"file": "mymodule.ex", "line": 17, "mfa": "mymodule:thefunction/1"},
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

You can specify multiple templates, so you can add your own metadata keys to one
of the standard templates, e.g., `[{keys, basic}, request_id, trace_id, span_id]`.


## Links

Much thanks to Fred Hebert, as always.

* [Erlang/OTP 21's new logger](https://ferd.ca/erlang-otp-21-s-new-logger.html)
* [flatlog](https://github.com/ferd/flatlog)
* [Canonical log lines](https://brandur.org/canonical-log-lines)
