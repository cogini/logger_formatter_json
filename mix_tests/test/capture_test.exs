defmodule LoggerFormatterJsonTest.CaptureTest do
  use ExUnit.Case
  import ExUnit.CaptureLog
  require Logger

  test "example" do
    {result, log} =
      with_log(fn ->
        Logger.error("log msg")
        2 + 2
      end)

    assert result == 4
    assert log =~ "log msg"
  end
end
