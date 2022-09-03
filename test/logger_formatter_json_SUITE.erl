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
       <<"{\"message\":\"abc\",\"syslog.severity\":\"info\"}">>,
       logger_formatter_json:format(#{level => info, msg => {string, "abc"},
                                      meta => #{}}, #{})),
    ?assertEqual(
       <<"{\"message\":\"abc\",\"syslog.severity\":\"info\"}">>,
       logger_formatter_json:format(#{level => info,
                                      msg => {string, [<<"abc">>]},
                                      meta => #{}}, #{})
    ),
    ?assertEqual(
       <<"{\"message\":\"hello world\",\"syslog.severity\":\"info\"}">>,
       logger_formatter_json:format(#{level => info,
                                      msg => {"hello ~s", ["world"]},
                                      meta => #{}}, #{})
    ),
    ok.

basic(_) ->
    ?assertEqual(
       <<"{\"hi\":\"there\",\"syslog.severity\":\"info\"}">>,
       logger_formatter_json:format(#{level => info, msg => {report, #{hi => there}}, meta => #{}}, #{})
    ),
    ?assertEqual(
       <<"{\"hi\":\"there\",\"syslog.severity\":\"info\"}">>,
       logger_formatter_json:format(#{level => info, msg => {report, #{hi => there}}, meta => #{}}, #{})
    ),
    ok.
