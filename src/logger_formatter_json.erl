%%% @doc
%%% This module formats logger events in JSON.
%%%
%%% https://www.erlang.org/doc/apps/kernel/logger_chapter.html#formatters
%%%
%%% logger library (part of the `kernel' application since OTP-21).
%%%
%%% The module honors the standard configuration of the kernel's default
%%% logger formatter regarding: max depth, templates.
%%% @end
-module(logger_formatter_json).

%% API exports
-export([format/2]).
% -export([check_config/1]).

-ifdef(TEST).
-export([format_msg/3]).
-endif.

%%%-----------------------------------------------------------------
%%% Types
-type config() :: #{chars_limit     => pos_integer() | unlimited,
                    depth           => pos_integer() | unlimited,
                    max_size        => pos_integer() | unlimited,
                    report_cb       => logger:report_cb(),
                    single_line     => boolean(),
                    time_designator => byte(),
                    time_offset     => integer() | [byte()]}.

%%====================================================================
%% API functions
%%====================================================================

-spec format(LogEvent, FConfig) -> unicode:chardata() when
      LogEvent :: logger:log_event(),
      FConfig :: config().

format(Map = #{msg := {report, #{label := {error_logger, _}, format := Format, args := Terms}}}, FConfig) ->
    format(Map#{msg := {report, #{message => unicode:characters_to_binary(io_lib:format(Format, Terms))}}}, FConfig);

format(#{level := Level, msg := {report, Msg}, meta := Meta0}, FConfig) when is_map(Msg) ->
    Config = add_default_config(FConfig),
    Meta = maps:merge(Meta0, #{level => Level}),
    format_msg(Msg, Meta, Config);

format(Map = #{msg := {report, KeyVal}}, FConfig) when is_list(KeyVal) ->
    format(Map#{msg := {report, maps:from_list(KeyVal)}}, FConfig);

format(Map = #{msg := {string, String}}, FConfig) ->
    format(Map#{msg := {report, #{message => unicode:characters_to_binary(String)}}}, FConfig);

format(Map = #{msg := {Format, Terms}}, FConfig) ->
    format(Map#{msg := {report, #{message => unicode:characters_to_binary(io_lib:format(Format, Terms))}}}, FConfig).

%%====================================================================
%% Internal functions
%%====================================================================
% -spec apply_defaults(FConfig) -> logger:formatter_config() when
%       FConfig :: logger:formatter_config().
% apply_defaults(FConfig) ->
%     maps:merge(
%       #{term_depth => undefined,
%         map_depth => -1,
%         time_offset => 0,
%         time_designator => $T
%        },
%       FConfig
%     ).

-spec format_msg(Msg, Meta, Config) -> unicode:chardata() when
      Msg :: map(),
      Meta :: map(),
      Config :: config().
format_msg(Msg, Meta, Config) ->
    Data = logger_formatter_json_datadog:format_msg(Msg, Meta, Config),
    thoas:encode(Data).

%% Ensure that all valid configuration parameters exist in the final
%% configuration map
-spec add_default_config(FConfig) -> logger:formatter_config() when
      FConfig :: logger:formatter_config().
add_default_config(Config0) ->
    Defaults = #{
        chars_limit => unlimited,
        error_logger_notice_header => info,
        single_line => true,
        time_designator => $T
    },
    MaxSize = get_max_size(maps:get(max_size, Config0, undefined)),
    Depth = get_depth(maps:get(depth, Config0, undefined)),
    Offset = get_offset(maps:get(time_offset, Config0, undefined)),
    maps:merge(Defaults, Config0#{max_size => MaxSize, depth => Depth,
                                  time_offset => Offset}).

get_max_size(undefined) ->
    unlimited;
get_max_size(S) ->
    max(10, S).

get_depth(undefined) ->
    error_logger:get_format_depth();
get_depth(S) ->
    max(5, S).

get_offset(undefined) ->
    utc_to_offset(get_utc_config());
get_offset(Offset) ->
    Offset.

utc_to_offset(true) ->
    "Z";
utc_to_offset(false) ->
    "".

get_utc_config() ->
    %% SASL utc_log overrides stdlib config - in order to have uniform
    %% timestamps in log messages
    case application:get_env(sasl, utc_log) of
        {ok, Val} when is_boolean(Val) -> Val;
        _ ->
            case application:get_env(stdlib, utc_log) of
                {ok, Val} when is_boolean(Val) -> Val;
                _ -> false
            end
    end.
