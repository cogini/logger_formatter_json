-module(logger_formatter_json_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [unstructured, basic].

unstructured() ->
    [{docs, "logs that aren't structured get passed through with a re-frame"}].
unstructured(_) ->
    ?assertEqual(
       <<"{\"syslog.severity\":\"info\",\"message\":\"abc\"}\n">>,
       iolist_to_binary(
         logger_formatter_json:format(#{level => info, msg => {string, "abc"},
                                        meta => #{}}, #{}))
      ),
    ?assertEqual(
       <<"{\"syslog.severity\":\"info\",\"message\":\"abc\"}\n">>,
       iolist_to_binary(
         logger_formatter_json:format(#{level => info,
                                        msg => {string, [<<"abc">>]},
                                        meta => #{}}, #{})
        )
      ),
    ?assertEqual(
       <<"{\"syslog.severity\":\"info\",\"message\":\"hello world\"}\n">>,
       iolist_to_binary(
         logger_formatter_json:format(#{level => info,
                                        msg => {"hello ~s", ["world"]},
                                        meta => #{}}, #{})
        )
      ),
    ok.

basic(_) ->
    ?assertEqual(
       <<"{\"syslog.severity\":\"info\",\"hi\":\"there\"}\n">>,
       iolist_to_binary(
         logger_formatter_json:format(#{level => info, msg => {report, #{hi => there}}, meta => #{}}, #{})
        )
      ),
    ok.
