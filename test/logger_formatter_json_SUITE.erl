-module(logger_formatter_json_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
  [
    print_string,
    to_string,
    printable_list,
    is_printable,
    unstructured,
    structured,
    metadata,
    duplicate_keys
  ].

print_string(_) ->
  Config = #{single_line => true},
  ?assertEqual(<<"foo">>, logger_formatter_json:print_string("foo", Config)),
  ?assertEqual(<<"foo">>, logger_formatter_json:print_string(<<"foo">>, Config)).


to_string(_) ->
  Config = #{single_line => true},
  ?assertEqual("foo", logger_formatter_json:to_string("foo", Config)),
  ?assertEqual("foo", logger_formatter_json:to_string(foo, Config)),
  ?assertEqual(["[]"], logger_formatter_json:to_string([], Config)),
  ?assertEqual(<<>>, logger_formatter_json:to_string(<<>>, Config)),
  ?assertEqual(<<"foo">>, logger_formatter_json:to_string(<<"foo">>, Config)),
  ?assertEqual(
    <<"foo\nbar">>,
    iolist_to_binary(logger_formatter_json:to_string(<<"foo\nbar">>, Config))
  ),
  ?assertEqual(
    <<"793µs"/utf8>>,
    iolist_to_binary(logger_formatter_json:to_string(<<"793µs"/utf8>>, Config))
  ),
  ?assertEqual("<0.250.0>", logger_formatter_json:to_string(c:pid(0, 250, 0), Config)).


printable_list(_) -> ?assertEqual(true, logger_formatter_json:printable_list("foo")).

is_printable(_) ->
  ?assertEqual(true, logger_formatter_json:is_printable(<<"foo">>)),
  % ?assertEqual(nomatch, re:run(<<"foo\nbar">>, <<"[[:^print:]]">>, [{capture, none}, unicode])),
  ?assertEqual(true, logger_formatter_json:is_printable(<<"foo\nbar">>)),
  ?assertEqual(true, logger_formatter_json:is_printable(<<"foo\nbar"/utf8>>)),
  ?assertEqual(false, logger_formatter_json:is_printable(<<0>>)).


unstructured() -> [{docs, "logs that aren't structured get passed through with a re-frame"}].

unstructured(_) ->
  Config = #{single_line => true},
  ?assertEqual(
    <<"{\"msg\":\"abc\",\"level\":\"info\"}\n">>,
    iolist_to_binary(
      logger_formatter_json:format(#{level => info, msg => {string, "abc"}, meta => #{}}, Config)
    )
  ),
  ?assertEqual(
    <<"{\"msg\":\"abc\",\"level\":\"info\"}\n">>,
    iolist_to_binary(
      logger_formatter_json:format(
        #{level => info, msg => {string, [<<"abc">>]}, meta => #{}},
        Config
      )
    )
  ),
  ?assertEqual(
    <<"{\"msg\":\"793\\u00B5s\",\"level\":\"info\"}\n">>,
    iolist_to_binary(
      logger_formatter_json:format(
        #{level => info, msg => {string, [<<"793µs"/utf8>>]}, meta => #{}},
        Config
      )
    )
  ),
  ?assertEqual(
    <<"{\"msg\":\"\",\"level\":\"info\"}\n">>,
    iolist_to_binary(
      logger_formatter_json:format(#{level => info, msg => {string, <<>>}, meta => #{}}, Config)
    )
  ),
  ?assertEqual(
    <<"{\"msg\":\"foo\\nbar\",\"level\":\"info\"}\n">>,
    iolist_to_binary(
      logger_formatter_json:format(
        #{level => info, msg => {string, <<"foo\nbar">>}, meta => #{}},
        Config
      )
    )
  ),
  ?assertEqual(
    <<"{\"msg\":\"foo\\n\",\"level\":\"info\"}\n">>,
    iolist_to_binary(
      logger_formatter_json:format(
        #{level => info, msg => {string, <<"foo\n">>}, meta => #{}},
        Config
      )
    )
  ),
  ?assertEqual(
    <<"{\"msg\":\"793\\u00B5s\\n\",\"level\":\"info\"}\n">>,
    iolist_to_binary(
      logger_formatter_json:format(
        #{level => info, msg => {string, <<"793µs\n"/utf8>>}, meta => #{}},
        Config
      )
    )
  ),
  ?assertEqual(
    <<
      "{\"msg\":\"GET /phoenix/live_reload/socket/websocket - Sent 404 in 793\\u00B5s\",\"level\":\"info\"}\n"
    >>,
    iolist_to_binary(
      logger_formatter_json:format(
        #{
          level => info,
          msg
          =>
          {
            string,
            [
              [<<"GET">>, 32, <<"/phoenix/live_reload/socket/websocket">>],
              <<" - ">>,
              <<"Sent">>,
              32,
              <<"404">>,
              <<" in ">>,
              <<"793µs"/utf8>>
            ]
          },
          meta => #{}
        },
        #{}
      )
    )
  ),
  ?assertEqual(
    <<"{\"msg\":\"hello world\",\"level\":\"info\"}\n">>,
    iolist_to_binary(
      logger_formatter_json:format(
        #{level => info, msg => {"hello ~s", ["world"]}, meta => #{}},
        #{}
      )
    )
  ),
  ?assertEqual(
    <<"{\"msg\":\"hello world\",\"level\":\"info\",\"request_id\":\"F6R64Fh3F9NzEscAAAaB\"}\n">>,
    iolist_to_binary(
      logger_formatter_json:format(
        #{
          level => info,
          msg => {"hello ~s", ["world"]},
          meta => #{request_id => <<"F6R64Fh3F9NzEscAAAaB">>}
        },
        #{template => [msg, level, request_id]}
      )
    )
  ),
  ?assertEqual(
    <<"{\"msg\":\"hello world\",\"level\":\"info\",\"request_id\":\"string with spaces\"}\n">>,
    iolist_to_binary(
      logger_formatter_json:format(
        #{
          level => info,
          msg => {"hello ~s", ["world"]},
          meta => #{request_id => <<"string with spaces">>}
        },
        #{template => [msg, level, request_id]}
      )
    )
  ),
  % Binary data
  ?assertEqual(
    <<"{\"msg\":\"hello world\",\"level\":\"info\",\"foo\":\"<<0,1,2,3>>\"}\n">>,
    iolist_to_binary(
      logger_formatter_json:format(
        #{level => info, msg => {"hello ~s", ["world"]}, meta => #{foo => <<0, 1, 2, 3>>}},
        #{template => [msg, level, foo]}
      )
    )
  ),
  ?assertEqual(
    <<
      "{\"msg\":\"Postgrex.Protocol (<0.6341.0>) failed to connect: ** (DBConnection.ConnectionError) tcp connect (postgres:5432): timeout\",\"level\":\"info\"}\n"
    >>,
    iolist_to_binary(
      logger_formatter_json:format(
        #{
          level => info,
          msg
          =>
          {
            string,
            [
              <<"Postgrex.Protocol">>,
              32,
              40,
              c:pid(0, 6341, 0),
              <<") failed to connect: ">>
              | <<"** (DBConnection.ConnectionError) tcp connect (postgres:5432): timeout">>
            ]
          },
          meta => #{}
        },
        Config
      )
    )
  ),
  ?assertEqual(
    <<
      "{\"msg\":\"<0.5054.0> running ExampleWeb.Endpoint (connection <0.5053.0>, stream id 1) terminated\\nServer: app.example.com:80 (http)\\nRequest: GET /dash/listings/44\\n** (exit) an exception was raised:\\n    ** (KeyError) key :prod_identifier not found in: %{\\n  id: \\\"gid://shopify/ProductVariant/42739636764833\\\",\\n  description: \\\"Example\\\"\\n}\\n        (example 0.1.0) lib/example_web/controllers/dash_controller.ex:58: anonymous fn/2 in ExampleWeb.DashController.admin_listing_page/2\\n        (elixir 1.17.2) lib/enum.ex:4301: Enum.filter_list/2\\n        (example 0.1.0) lib/example_web/controllers/dash_controller.ex:58: ExampleWeb.DashController.admin_listing_page/2\\n        (example 0.1.0) lib/example_web/controllers/dash_controller.ex:1: ExampleWeb.DashController.action/2\\n        (example 0.1.0) lib/example_web/controllers/dash_controller.ex:1: ExampleWeb.DashController.phoenix_controller_pipeline/2\\n        (phoenix 1.7.14) lib/phoenix/router.ex:484: Phoenix.Router.__call__/5\\n        (example 0.1.0) deps/plug/lib/plug/error_handler.ex:80: ExampleWeb.Router.call/2\\n        (example 0.1.0) lib/example_web/endpoint.ex:1: ExampleWeb.Endpoint.plug_builder_call/2\",\"level\":\"info\"}\n"
    >>,
    iolist_to_binary(
      logger_formatter_json:format(
        #{
          level => info,
          msg
          =>
          {
            string,
            [
              c:pid(0, 5054, 0),
              <<" running ">>,
              <<"ExampleWeb.Endpoint">>,
              [<<" (connection ">>, c:pid(0, 5053, 0), <<", stream id ">>, <<"1">>, 41],
              <<" terminated\n">>,
              [
                [
                  <<"Server: ">>,
                  <<"app.example.com">>,
                  <<":">>,
                  <<"80">>,
                  32,
                  40,
                  <<"http">>,
                  41,
                  10
                ],
                [<<"Request: ">>, <<"GET">>, 32, <<"/dash/listings/44">>, 10]
              ]
              |
              <<
                "** (exit) an exception was raised:\n    ** (KeyError) key :prod_identifier not found in: %{\n  id: \"gid://shopify/ProductVariant/42739636764833\",\n  description: \"Example\"\n}\n        (example 0.1.0) lib/example_web/controllers/dash_controller.ex:58: anonymous fn/2 in ExampleWeb.DashController.admin_listing_page/2\n        (elixir 1.17.2) lib/enum.ex:4301: Enum.filter_list/2\n        (example 0.1.0) lib/example_web/controllers/dash_controller.ex:58: ExampleWeb.DashController.admin_listing_page/2\n        (example 0.1.0) lib/example_web/controllers/dash_controller.ex:1: ExampleWeb.DashController.action/2\n        (example 0.1.0) lib/example_web/controllers/dash_controller.ex:1: ExampleWeb.DashController.phoenix_controller_pipeline/2\n        (phoenix 1.7.14) lib/phoenix/router.ex:484: Phoenix.Router.__call__/5\n        (example 0.1.0) deps/plug/lib/plug/error_handler.ex:80: ExampleWeb.Router.call/2\n        (example 0.1.0) lib/example_web/endpoint.ex:1: ExampleWeb.Endpoint.plug_builder_call/2"
              >>
            ]
          },
          meta => #{}
        },
        Config
      )
    )
  ),
  ok.


duplicate_keys(_) ->
  ?assertEqual(
    <<"{\"msg\":\"hello world\",\"level\":\"info\",\"foo\":\"bar\"}\n">>,
    iolist_to_binary(
      logger_formatter_json:format(
        #{level => info, msg => {"hello ~s", ["world"]}, meta => #{foo => "bar"}},
        #{template => [msg, level, foo, foo]}
      )
    )
  ),
  ok.


structured(_) ->
  ?assertEqual(
    <<"{\"msg\":{\"hi\":\"there\"},\"level\":\"info\"}\n">>,
    iolist_to_binary(
      logger_formatter_json:format(
        #{level => info, msg => {report, #{hi => there}}, meta => #{}},
        #{}
      )
    )
  ),
  ?assertEqual(
    <<
      "{\"msg\":{\"args\":\"10.10.2.182\",\"label\":\"{error_logger,error_msg}\",\"format\":\"** System NOT running to use fully qualified hostnames **~n** Hostname ~ts is illegal **~n\"},\"level\":\"info\"}\n"
    >>,
    iolist_to_binary(
      logger_formatter_json:format(
        #{
          level => info,
          msg
          =>
          {
            report,
            #{
              args => ["10.10.2.182"],
              label => {error_logger, error_msg},
              format
              =>
              "** System NOT running to use fully qualified hostnames **~n** Hostname ~ts is illegal **~n"
            }
          },
          meta => #{}
        },
        #{template => [msg, level, label, format]}
      )
    )
  ),
  % report_cb callback fun ignored for structured logs
  ?assertEqual(
    <<"{\"msg\":{\"hi\":\"there\"},\"level\":\"info\"}\n">>,
    iolist_to_binary(
      logger_formatter_json:format(
        #{level => info, msg => {report, #{hi => there}}, meta => #{}},
        #{report_cb => fun (_) -> {"ho ho ho", []} end}
      )
    )
  ),
  % Metadata with map value is embedded as map value
  ?assertEqual(
    <<"{\"msg\":{\"hi\":\"there\"},\"level\":\"info\",\"foo\":{\"biz\":\"baz\"}}\n">>,
    iolist_to_binary(
      logger_formatter_json:format(
        #{level => info, msg => {report, #{hi => there}}, meta => #{foo => #{biz => baz}}},
        #{template => [msg, level, rest]}
      )
    )
  ),
  ?assertEqual(
    <<"{\"level\":\"info\",\"hi\":\"there\"}\n">>,
    iolist_to_binary(
      logger_formatter_json:format(
        #{level => info, msg => {report, #{hi => there}}, meta => #{}},
        #{map_msg => merge, template => [msg, level, rest]}
      )
    )
  ),
  ?assertEqual(
    <<"{\"level\":\"info\",\"biz\":\"baz\"}\n">>,
    iolist_to_binary(
      logger_formatter_json:format(
        #{level => info, msg => {report, #{foo => bar, biz => baz}}, meta => #{}},
        #{map_msg => merge, template => [level, biz]}
      )
    )
  ),
  ok.


metadata(_) ->
  Config = #{names => datadog},
  ?assertEqual(
    <<"{\"message\":\"abc\",\"status\":\"info\"}\n">>,
    iolist_to_binary(
      logger_formatter_json:format(#{level => info, msg => {string, "abc"}, meta => #{}}, Config)
    )
  ),
  ok.
