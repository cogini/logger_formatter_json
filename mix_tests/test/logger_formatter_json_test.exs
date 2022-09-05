defmodule LoggerFormatterJsonTest do
  use ExUnit.Case

  describe "Unstructured log messages" do
    test "String with a charlist" do
       expected = ~s({"msg":"abc","level":"info"}\n)
       assert expected == to_string(:logger_formatter_json.format(%{level: :info, msg: {:string, 'abc'}, meta: %{}}, %{}))
    end

    test "String with a binary" do
       expected = ~s({"msg":"abc","level":"info"}\n)
       assert expected == to_string(:logger_formatter_json.format(%{level: :info, msg: {:string, "abc"}, meta: %{}}, %{}))
    end

    test "List of charlists" do
       expected = ~s({"msg":"abc","level":"info"}\n)
       assert expected == to_string(:logger_formatter_json.format(%{level: :info, msg: {:string, ['abc']}, meta: %{}}, %{}))
    end

    test "Erlang format string and args" do
       expected = ~s({"msg":"hello world","level":"info"}\n)
       assert expected == to_string(:logger_formatter_json.format(%{level: :info, msg: {'hello ~s', ['world']}, meta: %{}}, %{}))
    end
  end

  describe "Structured log messages" do
    test "Simple map" do
       expected = ~s({"hi":"there","level":"info"}\n)
       assert expected == to_string(:logger_formatter_json.format(%{level: :info, msg: {:report, %{hi: :there}}, meta: %{}}, %{}))
    end
  end
end
