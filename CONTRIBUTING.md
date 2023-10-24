## Contributing

Build:

```console
rebar3 compile
```

Test:

```console
rebar3 ct
```

There are also tests written in Elixir. Change to the `mix_tests` directory and run:

```console
MIX_ENV=test mix deps.get
mix test
```

Format code:

```console
rebar3 steamroll
```

Generate docs:

```console
rebar3 ex_docs
```

Publish:

```console
rebar3 hex user auth
rebar3 hex build
rebar3 hex publish
```

This project uses the Contributor Covenant version 2.1. Check [CODE_OF_CONDUCT.md](/CODE_OF_CONDUCT.md) for more information.
