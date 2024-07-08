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
  ).


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
        #{}
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
