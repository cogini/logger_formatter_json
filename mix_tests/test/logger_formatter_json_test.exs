defmodule LoggerFormatterJsonTest do
  use ExUnit.Case

  describe "Unstructured log messages" do
    test "String with a charlist" do
      expected = ~s({"msg":"abc","level":"info"}\n)

      assert expected ==
               to_string(
                 :logger_formatter_json.format(
                   %{level: :info, msg: {:string, ~c"abc"}, meta: %{}},
                   %{}
                 )
               )
    end

    test "String with a binary" do
      expected = ~s({"msg":"abc","level":"info"}\n)

      assert expected ==
               to_string(
                 :logger_formatter_json.format(
                   %{level: :info, msg: {:string, "abc"}, meta: %{}},
                   %{}
                 )
               )
    end

    test "List of charlists" do
      expected = ~s({"msg":"abc","level":"info"}\n)

      assert expected ==
               to_string(
                 :logger_formatter_json.format(
                   %{level: :info, msg: {:string, [~c"abc"]}, meta: %{}},
                   %{}
                 )
               )
    end

    test "Erlang format string and args" do
      expected = ~s({"msg":"hello world","level":"info"}\n)

      assert expected ==
               to_string(
                 :logger_formatter_json.format(
                   %{level: :info, msg: {~c"hello ~s", [~c"world"]}, meta: %{}},
                   %{}
                 )
               )
    end

    test "String with microsecond" do
      expected = ~s({"msg":"408\\u00B5s","level":"info"}\n)

      assert expected ==
               to_string(
                 :logger_formatter_json.format(
                   %{level: :info, msg: {:string, "408µs"}, meta: %{}},
                   %{}
                 )
               )
    end

    test "String with new line after microsecond" do
      expected = ~s({"msg":"408\\u00B5s\\n","level":"info"}\n)

      assert expected ==
               to_string(
                 :logger_formatter_json.format(
                   %{level: :info, msg: {:string, "408µs\n"}, meta: %{}},
                   %{}
                 )
               )
    end
  end

  describe "Structured log messages" do
    test "Simple map" do
      expected = ~s({"msg":{"hi":"there"},"level":"info"}\n)

      assert expected ==
               to_string(
                 :logger_formatter_json.format(
                   %{level: :info, msg: {:report, %{hi: :there}}, meta: %{}},
                   %{}
                 )
               )
    end
  end

  describe "iodata" do
    test "String with a binary" do
      expected = ~s"""
      {"msg":"GenServer ExAws.Config.AuthCache terminating\\n** (RuntimeError) Instance Meta Error: {:error, %{reason: :timeout}}\\n\\nYou tried to access the AWS EC2 instance meta, but it could not be reached.\\nThis happens most often when trying to access it from your local computer,\\nwhich happens when environment variables are not set correctly prompting\\nExAws to fallback to the Instance Meta.\\n\\nPlease check your key config and make sure they're configured correctly:\\n\\nFor Example:\\n```\\nExAws.Config.new(:s3)\\nExAws.Config.new(:dynamodb)\\n```\\n\\n    (ex_aws 2.5.4) lib/ex_aws/instance_meta.ex:27: ExAws.InstanceMeta.request/3\\n    (ex_aws 2.5.4) lib/ex_aws/instance_meta.ex:84: ExAws.InstanceMeta.instance_role_credentials/1\\n    (ex_aws 2.5.4) lib/ex_aws/instance_meta.ex:92: ExAws.InstanceMeta.security_credentials/1\\n    (ex_aws 2.5.4) lib/ex_aws/config/auth_cache.ex:132: ExAws.Config.AuthCache.refresh_auth_now/2\\n    (ex_aws 2.5.4) lib/ex_aws/config/auth_cache.ex:45: ExAws.Config.AuthCache.handle_call/3\\n    (stdlib 6.0) gen_server.erl:2209: :gen_server.try_handle_call/4\\n    (stdlib 6.0) gen_server.erl:2238: :gen_server.handle_msg/6\\n    (stdlib 6.0) proc_lib.erl:329: :proc_lib.init_p_do_apply/3\\nLast message (from #PID<0.3700.0>): {:refresh_auth, %{port: 443, scheme: \\"https://\\", host: \\"ec2.us-east-1.amazonaws.com\\", http_client: ExAws.Request.Hackney, access_key_id: [{:system, \\"AWS_ACCESS_KEY_ID\\"}, :instance_role], region: \\"us-east-1\\", secret_access_key: [{:system, \\"AWS_SECRET_ACCESS_KEY\\"}, :instance_role], retries: [max_attempts: 10, base_backoff_in_ms: 10, max_backoff_in_ms: 10000], json_codec: Jason, normalize_path: true, require_imds_v2: false}}","level":"info"}
      """

      iodata = [<<"GenServer ">>,<<"ExAws.Config.AuthCache">>,<<" terminating">>,[[10|<<"** (RuntimeError) Instance Meta Error: {:error, %{reason: :timeout}}\n\nYou tried to access the AWS EC2 instance meta, but it could not be reached.\nThis happens most often when trying to access it from your local computer,\nwhich happens when environment variables are not set correctly prompting\nExAws to fallback to the Instance Meta.\n\nPlease check your key config and make sure they're configured correctly:\n\nFor Example:\n```\nExAws.Config.new(:s3)\nExAws.Config.new(:dynamodb)\n```\n">>],[<<"\n    ">>|<<"(ex_aws 2.5.4) lib/ex_aws/instance_meta.ex:27: ExAws.InstanceMeta.request/3">>],[<<"\n    ">>|<<"(ex_aws 2.5.4) lib/ex_aws/instance_meta.ex:84: ExAws.InstanceMeta.instance_role_credentials/1">>],[<<"\n    ">>|<<"(ex_aws 2.5.4) lib/ex_aws/instance_meta.ex:92: ExAws.InstanceMeta.security_credentials/1">>],[<<"\n    ">>|<<"(ex_aws 2.5.4) lib/ex_aws/config/auth_cache.ex:132: ExAws.Config.AuthCache.refresh_auth_now/2">>],[<<"\n    ">>|<<"(ex_aws 2.5.4) lib/ex_aws/config/auth_cache.ex:45: ExAws.Config.AuthCache.handle_call/3">>],[<<"\n    ">>|<<"(stdlib 6.0) gen_server.erl:2209: :gen_server.try_handle_call/4">>],[<<"\n    ">>|<<"(stdlib 6.0) gen_server.erl:2238: :gen_server.handle_msg/6">>],[<<"\n    ">>|<<"(stdlib 6.0) proc_lib.erl:329: :proc_lib.init_p_do_apply/3">>]],[],<<"\nLast message">>,[<<" (from ">>,<<"#PID<0.3700.0>">>,<<")">>],<<": ">>,<<"{:refresh_auth, %{port: 443, scheme: \"https://\", host: \"ec2.us-east-1.amazonaws.com\", http_client: ExAws.Request.Hackney, access_key_id: [{:system, \"AWS_ACCESS_KEY_ID\"}, :instance_role], region: \"us-east-1\", secret_access_key: [{:system, \"AWS_SECRET_ACCESS_KEY\"}, :instance_role], retries: [max_attempts: 10, base_backoff_in_ms: 10, max_backoff_in_ms: 10000], json_codec: Jason, normalize_path: true, require_imds_v2: false}}">>]
      assert expected ==
               to_string(
                 :logger_formatter_json.format(
                   %{level: :info, msg: {:string, iodata}, meta: %{}},
                   %{}
                 )
               )
    end
end

  describe "crash" do
    test "String with a binary" do
      expected = ~s"""
      {\"msg\":\"<0.767.0> running Phoenix.Endpoint.SyncCodeReloadPlug (connection <0.766.0>, stream id 1) terminated\\nServer: localhost:4000 (http)\\nRequest: GET /\\n** (exit) an exception was raised:\\n    ** (ArithmeticError) bad argument in arithmetic expression\\n        (erts 15.0) :erlang.+(1, :a)\\n        (phoenix_container_example 0.1.0) lib/phoenix_container_example_web/controllers/page_controller.ex:8: PhoenixContainerExampleWeb.PageController.home/2\\n        (phoenix_container_example 0.1.0) lib/phoenix_container_example_web/controllers/page_controller.ex:1: PhoenixContainerExampleWeb.PageController.action/2\\n        (phoenix_container_example 0.1.0) lib/phoenix_container_example_web/controllers/page_controller.ex:1: PhoenixContainerExampleWeb.PageController.phoenix_controller_pipeline/2\\n        (phoenix 1.7.14) lib/phoenix/router.ex:484: Phoenix.Router.__call__/5\\n        (phoenix_container_example 0.1.0) lib/phoenix_container_example_web/endpoint.ex:1: PhoenixContainerExampleWeb.Endpoint.plug_builder_call/2\\n        (phoenix_container_example 0.1.0) deps/plug/lib/plug/debugger.ex:136: PhoenixContainerExampleWeb.Endpoint.\\\"call (overridable 3)\\\"/2\\n        (phoenix_container_example 0.1.0) lib/phoenix_container_example_web/endpoint.ex:1: PhoenixContainerExampleWeb.Endpoint.call/2\\n        (phoenix 1.7.14) lib/phoenix/endpoint/sync_code_reload_plug.ex:22: Phoenix.Endpoint.SyncCodeReloadPlug.do_call/4\\n        (plug_cowboy 2.7.1) lib/plug/cowboy/handler.ex:11: Plug.Cowboy.Handler.init/2\\n        (cowboy 2.12.0) /Users/jake/work/phoenix_container_example/deps/cowboy/src/cowboy_handler.erl:37: :cowboy_handler.execute/2\\n        (cowboy 2.12.0) /Users/jake/work/phoenix_container_example/deps/cowboy/src/cowboy_stream_h.erl:306: :cowboy_stream_h.execute/3\\n        (cowboy 2.12.0) /Users/jake/work/phoenix_container_example/deps/cowboy/src/cowboy_stream_h.erl:295: :cowboy_stream_h.request_process/3\\n        (stdlib 6.0) proc_lib.erl:329: :proc_lib.init_p_do_apply/3\",\"level\":\"info\"}
      """

      msg = {:string,[:c.pid(0, 767, 0),<<" running ">>,
         <<"Phoenix.Endpoint.SyncCodeReloadPlug">>,
         [<<" (connection ">>,:c.pid(0, 766, 0),<<", stream id ">>,<<"1">>,
          41],
         <<" terminated\n">>,
         [[<<"Server: ">>,<<"localhost">>,<<":">>,<<"4000">>,32,40,<<"http">>,
           41,10],
          [<<"Request: ">>,<<"GET">>,32,<<"/">>,10]]|
         <<"** (exit) an exception was raised:\n    ** (ArithmeticError) bad argument in arithmetic expression\n        (erts 15.0) :erlang.+(1, :a)\n        (phoenix_container_example 0.1.0) lib/phoenix_container_example_web/controllers/page_controller.ex:8: PhoenixContainerExampleWeb.PageController.home/2\n        (phoenix_container_example 0.1.0) lib/phoenix_container_example_web/controllers/page_controller.ex:1: PhoenixContainerExampleWeb.PageController.action/2\n        (phoenix_container_example 0.1.0) lib/phoenix_container_example_web/controllers/page_controller.ex:1: PhoenixContainerExampleWeb.PageController.phoenix_controller_pipeline/2\n        (phoenix 1.7.14) lib/phoenix/router.ex:484: Phoenix.Router.__call__/5\n        (phoenix_container_example 0.1.0) lib/phoenix_container_example_web/endpoint.ex:1: PhoenixContainerExampleWeb.Endpoint.plug_builder_call/2\n        (phoenix_container_example 0.1.0) deps/plug/lib/plug/debugger.ex:136: PhoenixContainerExampleWeb.Endpoint.\"call (overridable 3)\"/2\n        (phoenix_container_example 0.1.0) lib/phoenix_container_example_web/endpoint.ex:1: PhoenixContainerExampleWeb.Endpoint.call/2\n        (phoenix 1.7.14) lib/phoenix/endpoint/sync_code_reload_plug.ex:22: Phoenix.Endpoint.SyncCodeReloadPlug.do_call/4\n        (plug_cowboy 2.7.1) lib/plug/cowboy/handler.ex:11: Plug.Cowboy.Handler.init/2\n        (cowboy 2.12.0) /Users/jake/work/phoenix_container_example/deps/cowboy/src/cowboy_handler.erl:37: :cowboy_handler.execute/2\n        (cowboy 2.12.0) /Users/jake/work/phoenix_container_example/deps/cowboy/src/cowboy_stream_h.erl:306: :cowboy_stream_h.execute/3\n        (cowboy 2.12.0) /Users/jake/work/phoenix_container_example/deps/cowboy/src/cowboy_stream_h.erl:295: :cowboy_stream_h.request_process/3\n        (stdlib 6.0) proc_lib.erl:329: :proc_lib.init_p_do_apply/3">>]}

      assert expected ==
               to_string(
                 :logger_formatter_json.format(
                   %{level: :info, msg: msg, meta: %{}},
                   %{}
                 )
               )
    end
  end
end
