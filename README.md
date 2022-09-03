logger_formatter_json
=====

A formatter for the Erlang logger application that outputs JSON.

The logger application was introduced in OTP 21.
https://www.erlang.org/doc/apps/kernel/logger_chapter.html#formatters



Build
-----

```console
rebar3 compile
```

Usage
-----

For Erlang, configure the default handler to your `sys.config` file:

```erlang
[
 {kernel, [
    {logger, [
        {handler, default, logger_std_h,
         #{formatter => {logger_formatter_json, #{
            map_depth => 3,
            term_depth => 50
          }}}
        }
    ]},
    {logger_level, info}
 ]}
].
```

Test
----

```console
rebar3 check
```
