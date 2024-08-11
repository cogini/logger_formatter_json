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
  ?assertEqual(
    <<"{\"msg\":\"<0.4184.0> running ExampleWeb.Endpoint (connection <0.4173.0>, stream id 2) terminated\\nServer: app.example.com:80 (http)\\nRequest: GET /dash/listings/44\\n** (exit) an exception was raised:\\n ** (KeyError) key :prod_identifier not found in: %{\\n id: \\\"gid://shopify/ProductVariant/42739636764833\\\",\\n description: \\\"Example\\\",\\n title: \\\"MLS Only SB\\\",\\n identifier: \\\"mls_only_7.21\\\",\\n order: 2,\\n subtitle: \\\"Default Title\\\",\\n price: 99,\\n sku: \\\"mls_only_7.21\\\",\\n product_image_src: \\\"https://cdn.shopify.com/s/files/1/0584/4444/7905/products/Group1600mlsonly.png?v=1669771810\\\"\\n}\\n (homepie 0.1.0) lib/homepie_web/controllers/dash_controller.ex:58: anonymous fn/2 in ExampleWeb.DashController.admin_listing_page/2\\n (elixir 1.17.2) lib/enum.ex:4301: Enum.filter_list/2\\n (homepie 0.1.0) lib/homepie_web/controllers/dash_controller.ex:58: ExampleWeb.DashController.admin_listing_page/2\\n (homepie 0.1.0) lib/homepie_web/controllers/dash_controller.ex:1: ExampleWeb.DashController.action/2\\n (homepie 0.1.0) lib/homepie_web/controllers/dash_controller.ex:1: ExampleWeb.DashController.phoenix_controller_pipeline/2\\n (phoenix 1.7.14) lib/phoenix/router.ex:484: Phoenix.Router.__call__/5\\n (homepie 0.1.0) deps/plug/lib/plug/error_handler.ex:80: ExampleWeb.Router.call/2\\n (homepie 0.1.0) lib/homepie_web/endpoint.ex:1: ExampleWeb.Endpoint.plug_builder_call/2\",\"level\":\"info\"}\n">>,
    iolist_to_binary(
      logger_formatter_json:format(
        #{
          level => info,
          msg
          =>
{string,[c:pid(0,4184,0),<<" running ">>,<<"ExampleWeb.Endpoint">>,[<<" (connection ">>,c:pid(0,4173,0),<<", stream id ">>,<<"2">>,41],<<" terminated\n">>,[[<<"Server: ">>,<<"app.example.com">>,<<":">>,<<"80">>,32,40,<<"http">>,41,10],[<<"Request: ">>,<<"GET">>,32,<<"/dash/listings/44">>,10]]|<<"** (exit) an exception was raised:\n ** (KeyError) key :prod_identifier not found in: %{\n id: \"gid://shopify/ProductVariant/42739636764833\",\n description: \"Example\",\n title: \"MLS Only SB\",\n identifier: \"mls_only_7.21\",\n order: 2,\n subtitle: \"Default Title\",\n price: 99,\n sku: \"mls_only_7.21\",\n product_image_src: \"https://cdn.shopify.com/s/files/1/0584/4444/7905/products/Group1600mlsonly.png?v=1669771810\"\n}\n (homepie 0.1.0) lib/homepie_web/controllers/dash_controller.ex:58: anonymous fn/2 in ExampleWeb.DashController.admin_listing_page/2\n (elixir 1.17.2) lib/enum.ex:4301: Enum.filter_list/2\n (homepie 0.1.0) lib/homepie_web/controllers/dash_controller.ex:58: ExampleWeb.DashController.admin_listing_page/2\n (homepie 0.1.0) lib/homepie_web/controllers/dash_controller.ex:1: ExampleWeb.DashController.action/2\n (homepie 0.1.0) lib/homepie_web/controllers/dash_controller.ex:1: ExampleWeb.DashController.phoenix_controller_pipeline/2\n (phoenix 1.7.14) lib/phoenix/router.ex:484: Phoenix.Router.__call__/5\n (homepie 0.1.0) deps/plug/lib/plug/error_handler.ex:80: ExampleWeb.Router.call/2\n (homepie 0.1.0) lib/homepie_web/endpoint.ex:1: ExampleWeb.Endpoint.plug_builder_call/2">>]},
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
    <<"{\"msg\":{\"args\":\"10.10.2.182\",\"format\":\"** System NOT running to use fully qualified hostnames **~n** Hostname ~ts is illegal **~n\",\"label\":\"{error_logger,error_msg}\"},\"level\":\"info\"}\n">>,
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
  Config = #{template => [msg, level, conn,crash_reason,domain,error_logger,gl,otel_span_id,
 otel_trace_flags,otel_trace_id,pid,time,xray_trace_id]},
  Meta = #{error_logger => #{tag => error},pid => c:pid(0, 672, 0),time => 1723325773127320,gl => c:pid(0, 394, 0),domain => [cowboy],otel_trace_id => "66b7dd4d7dad9f9aba84fb43389b4611",otel_span_id => "052d4929d398aec6",xray_trace_id => <<"1-66b7dd4d-dad9f9aba84fb43389b4611@052d4929d398aec6">>,otel_trace_flags => "01",crash_reason => {#{message => <<"bad argument in arithmetic expression">>,'__struct__' => 'Elixir.ArithmeticError','__exception__' => true},[{erlang,'+',[1,a],[{error_info,#{module => erl_erts_errors}}]},{'Elixir.PhoenixContainerExampleWeb.PageController',home,2,[{file,"lib/phoenix_container_example_web/controllers/page_controller.ex"},{line,8}]},{'Elixir.PhoenixContainerExampleWeb.PageController',action,2,[{file,"lib/phoenix_container_example_web/controllers/page_controller.ex"},{line,1}]},{'Elixir.PhoenixContainerExampleWeb.PageController',phoenix_controller_pipeline,2,[{file,"lib/phoenix_container_example_web/controllers/page_controller.ex"},{line,1}]},{'Elixir.Phoenix.Router','__call__',5,[{file,"lib/phoenix/router.ex"},{line,484}]},{'Elixir.PhoenixContainerExampleWeb.Endpoint',plug_builder_call,2,[{file,"lib/phoenix_container_example_web/endpoint.ex"},{line,1}]},{'Elixir.PhoenixContainerExampleWeb.Endpoint','call (overridable 3)',2,[{file,"deps/plug/lib/plug/debugger.ex"},{line,136}]},{'Elixir.PhoenixContainerExampleWeb.Endpoint',call,2,[{file,"lib/phoenix_container_example_web/endpoint.ex"},{line,1}]},{'Elixir.Phoenix.Endpoint.SyncCodeReloadPlug',do_call,4,[{file,"lib/phoenix/endpoint/sync_code_reload_plug.ex"},{line,22}]},{'Elixir.Plug.Cowboy.Handler',init,2,[{file,"lib/plug/cowboy/handler.ex"},{line,11}]},{cowboy_handler,execute,2,[{file,"/Users/jake/work/phoenix_container_example/deps/cowboy/src/cowboy_handler.erl"},{line,37}]},{cowboy_stream_h,execute,3,[{file,"/Users/jake/work/phoenix_container_example/deps/cowboy/src/cowboy_stream_h.erl"},{line,306}]},{cowboy_stream_h,request_process,3,[{file,"/Users/jake/work/phoenix_container_example/deps/cowboy/src/cowboy_stream_h.erl"},{line,295}]},{proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,329}]}]},conn => #{owner => c:pid(0,673,0),port => 4000,private => #{},scheme => http,status => nil,script_name => [],state => unset,host => <<"localhost">>,params => #{'__struct__' => 'Elixir.Plug.Conn.Unfetched',aspect => params},'__struct__' => 'Elixir.Plug.Conn',halted => false,adapter => {'Elixir.Plug.Cowboy.Conn',#{pid => c:pid(0,672,0),port => 4000,scheme => <<"http">>,version => 'HTTP/1.1',path => <<"/">>,host => <<"localhost">>,peer => {{127,0,0,1},58196},bindings => #{},ref => 'Elixir.PhoenixContainerExampleWeb.Endpoint.HTTP',cert => undefined,headers => #{<<"accept">> => <<"*/*">>,<<"host">> => <<"localhost:4000">>,<<"user-agent">> => <<"curl/8.6.0">>},sock => {{127,0,0,1},4000},method => <<"GET">>,path_info => undefined,host_info => undefined,streamid => 1,body_length => 0,has_body => false,qs => <<>>}},secret_key_base => nil,cookies => #{'__struct__' => 'Elixir.Plug.Conn.Unfetched',aspect => cookies},request_path => <<"/">>,assigns => #{},method => <<"GET">>,query_string => <<>>,remote_ip => {127,0,0,1},req_headers => [{<<"accept">>,<<"*/*">>},{<<"host">>,<<"localhost:4000">>},{<<"user-agent">>,<<"curl/8.6.0">>}],path_info => [],path_params => #{},resp_headers => [{<<"cache-control">>,<<"max-age=0, private, must-revalidate">>}],resp_cookies => #{},resp_body => nil,body_params => #{'__struct__' => 'Elixir.Plug.Conn.Unfetched',aspect => body_params},query_params => #{'__struct__' => 'Elixir.Plug.Conn.Unfetched',aspect => query_params},req_cookies => #{'__struct__' => 'Elixir.Plug.Conn.Unfetched',aspect => cookies}}},
  ?assertEqual(
     <<"{\"msg\":\"abc\",\"level\":\"info\",\"conn\":{\"__struct__\":\"Elixir.Plug.Conn\",\"adapter\":\"{'Elixir.Plug.Cowboy.Conn',#{pid => <0.672.0>,port => 4000,scheme => <<\\\"http\\\">>,version => 'HTTP/1.1',path => <<\\\"/\\\">>,host => <<\\\"localhost\\\">>,peer => {{127,0,0,1},58196},bindings => #{},cert => undefined,headers => #{<<\\\"accept\\\">> => <<\\\"*/*\\\">>,<<\\\"host\\\">> => <<\\\"localhost:4000\\\">>,<<\\\"user-agent\\\">> => <<\\\"curl/8.6.0\\\">>},ref => 'Elixir.PhoenixContainerExampleWeb.Endpoint.HTTP',body_length => 0,has_body => false,host_info => undefined,method => <<\\\"GET\\\">>,path_info => undefined,qs => <<>>,sock => {{127,0,0,1},4000},streamid => 1}}\",\"assigns\":[],\"body_params\":{\"__struct__\":\"Elixir.Plug.Conn.Unfetched\",\"aspect\":\"body_params\"},\"cookies\":{\"__struct__\":\"Elixir.Plug.Conn.Unfetched\",\"aspect\":\"cookies\"},\"halted\":\"false\",\"host\":\"localhost\",\"method\":\"GET\",\"owner\":\"<0.673.0>\",\"params\":{\"__struct__\":\"Elixir.Plug.Conn.Unfetched\",\"aspect\":\"params\"},\"path_info\":\"[]\",\"path_params\":[],\"port\":\"4000\",\"private\":[],\"query_params\":{\"__struct__\":\"Elixir.Plug.Conn.Unfetched\",\"aspect\":\"query_params\"},\"query_string\":\"\",\"remote_ip\":\"{127,0,0,1}\",\"req_cookies\":{\"__struct__\":\"Elixir.Plug.Conn.Unfetched\",\"aspect\":\"cookies\"},\"req_headers\":\"[{<<\\\"accept\\\">>,<<\\\"*/*\\\">>},{<<\\\"host\\\">>,<<\\\"localhost:4000\\\">>},{<<\\\"user-agent\\\">>,<<\\\"curl/8.6.0\\\">>}]\",\"request_path\":\"/\",\"resp_body\":\"nil\",\"resp_cookies\":[],\"resp_headers\":\"[{<<\\\"cache-control\\\">>,<<\\\"max-age=0, private, must-revalidate\\\">>}]\",\"scheme\":\"http\",\"script_name\":\"[]\",\"secret_key_base\":\"nil\",\"state\":\"unset\",\"status\":\"nil\"},\"crash_reason\":\"{#{message => <<\\\"bad argument in arithmetic expression\\\">>,'__struct__' => 'Elixir.ArithmeticError','__exception__' => true},[{erlang,'+',[1,a],[{error_info,#{module => erl_erts_errors}}]},{'Elixir.PhoenixContainerExampleWeb.PageController',home,2,[{file,\\\"lib/phoenix_container_example_web/controllers/page_controller.ex\\\"},{line,8}]},{'Elixir.PhoenixContainerExampleWeb.PageController',action,2,[{file,\\\"lib/phoenix_container_example_web/controllers/page_controller.ex\\\"},{line,1}]},{'Elixir.PhoenixContainerExampleWeb.PageController',phoenix_controller_pipeline,2,[{file,\\\"lib/phoenix_container_example_web/controllers/page_controller.ex\\\"},{line,1}]},{'Elixir.Phoenix.Router','__call__',5,[{file,\\\"lib/phoenix/router.ex\\\"},{line,484}]},{'Elixir.PhoenixContainerExampleWeb.Endpoint',plug_builder_call,2,[{file,\\\"lib/phoenix_container_example_web/endpoint.ex\\\"},{line,1}]},{'Elixir.PhoenixContainerExampleWeb.Endpoint','call (overridable 3)',2,[{file,\\\"deps/plug/lib/plug/debugger.ex\\\"},{line,136}]},{'Elixir.PhoenixContainerExampleWeb.Endpoint',call,2,[{file,\\\"lib/phoenix_container_example_web/endpoint.ex\\\"},{line,1}]},{'Elixir.Phoenix.Endpoint.SyncCodeReloadPlug',do_call,4,[{file,\\\"lib/phoenix/endpoint/sync_code_reload_plug.ex\\\"},{line,22}]},{'Elixir.Plug.Cowboy.Handler',init,2,[{file,\\\"lib/plug/cowboy/handler.ex\\\"},{line,11}]},{cowboy_handler,execute,2,[{file,\\\"/Users/jake/work/phoenix_container_example/deps/cowboy/src/cowboy_handler.erl\\\"},{line,37}]},{cowboy_stream_h,execute,3,[{file,\\\"/Users/jake/work/phoenix_container_example/deps/cowboy/src/cowboy_stream_h.erl\\\"},{line,306}]},{cowboy_stream_h,request_process,3,[{file,\\\"/Users/jake/work/phoenix_container_example/deps/cowboy/src/cowboy_stream_h.erl\\\"},{line,295}]},{proc_lib,init_p_do_apply,3,[{file,\\\"proc_lib.erl\\\"},{line,329}]}]}\",\"domain\":\"[cowboy]\",\"error_logger\":{\"tag\":\"error\"},\"gl\":\"<0.394.0>\",\"otel_span_id\":\"052d4929d398aec6\",\"otel_trace_flags\":\"01\",\"otel_trace_id\":\"66b7dd4d7dad9f9aba84fb43389b4611\",\"pid\":\"<0.672.0>\",\"time\":\"2024-08-10T16:36:13.127320-05:00\",\"xray_trace_id\":\"1-66b7dd4d-dad9f9aba84fb43389b4611@052d4929d398aec6\"}\n">>,
    iolist_to_binary(
      logger_formatter_json:format(#{level => info, msg => {string, "abc"}, meta => Meta}, Config)
    )
  ),
  ok.
