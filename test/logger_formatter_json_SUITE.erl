-module(logger_formatter_json_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() -> [unstructured, structured, metadata, duplicate_keys].

unstructured() -> [{docs, "logs that aren't structured get passed through with a re-frame"}].

unstructured(_) ->
  ?assertEqual(
    <<"{\"msg\":\"abc\",\"level\":\"info\"}\n">>,
    iolist_to_binary(
      logger_formatter_json:format(#{level => info, msg => {string, "abc"}, meta => #{}}, #{})
    )
  ),
  ?assertEqual(
    <<"{\"msg\":\"abc\",\"level\":\"info\"}\n">>,
    iolist_to_binary(
      logger_formatter_json:format(#{level => info, msg => {string, [<<"abc">>]}, meta => #{}}, #{})
    )
  ),
  ?assertEqual(
    <<"{\"msg\":\"793\\u00B5s\",\"level\":\"info\"}\n">>,
    iolist_to_binary(
      logger_formatter_json:format(
        #{level => info, msg => {string, [<<"793µs"/utf8>>]}, meta => #{}},
        #{}
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
  ?assertEqual(
    <<"{\"msg\":\"hello world\",\"level\":\"info\",\"foo\":\"<<\\\"control char\\\\n\\\">>\"}\n">>,
    iolist_to_binary(
      logger_formatter_json:format(
        #{level => info, msg => {"hello ~s", ["world"]}, meta => #{foo => <<"control char\n">>}},
        #{template => [msg, level, foo]}
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
