-module(logger_formatter_json_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [unstructured, structured, metadata].

unstructured() ->
    [{docs, "logs that aren't structured get passed through with a re-frame"}].
unstructured(_) ->
    ?assertEqual(
       <<"{\"msg\":\"abc\",\"level\":\"info\"}\n">>,
       iolist_to_binary(
         logger_formatter_json:format(#{level => info, msg => {string, "abc"},
                                        meta => #{}}, #{}))
      ),
    ?assertEqual(
       <<"{\"msg\":\"abc\",\"level\":\"info\"}\n">>,
       iolist_to_binary(
         logger_formatter_json:format(#{level => info,
                                        msg => {string, [<<"abc">>]},
                                        meta => #{}}, #{})
        )
      ),
    ?assertEqual(
       <<"{\"msg\":\"hello world\",\"level\":\"info\"}\n">>,
       iolist_to_binary(
         logger_formatter_json:format(#{level => info,
                                        msg => {"hello ~s", ["world"]},
                                        meta => #{}}, #{})
        )
      ),
    ok.

structured(_) ->
    ?assertEqual(
       <<"{\"hi\":\"there\",\"level\":\"info\"}\n">>,
       iolist_to_binary(
         logger_formatter_json:format(#{level => info, msg => {report, #{hi => there}}, meta => #{}}, #{})
        )
      ),
    ok.

metadata(_) ->
    Config = #{
               names => datadog
              },
    ?assertEqual(
       <<"{\"message\":\"abc\",\"status\":\"info\"}\n">>,
       iolist_to_binary(
         logger_formatter_json:format(#{level => info, msg => {string, "abc"},
                                        meta => #{}}, Config))
      ),
    ok.
