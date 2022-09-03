%%% @doc
%%% Convert metadata to the format used by Datadog (https://www.datadoghq.com).
%%% This follows the default standard attribute list as much as possible.
%%% * https://docs.datadoghq.com/logs/log_configuration/attributes_naming_convention/#reserved-attributes
%%% * https://docs.datadoghq.com/logs/processing/attributes_naming_convention/#default-standard-attribute-list
%%%
%%% To connect logs and traces, span_id and trace_id metadata are rendered as dd.span_id and dd.trace_id.
%%% https://docs.datadoghq.com/tracing/faq/why-cant-i-see-my-correlated-logs-in-the-trace-id-panel/?tab=jsonlogs
%%%
%%% @end
-module(logger_formatter_json_datadog).

-export([format_msg/3]).

-spec format_msg(Msg, Meta, Config) -> map() when
      Msg :: map(),
      Meta :: map(),
      Config :: logger_formatter_config:config().
format_msg(Msg, Meta, Config) ->
    Data0 = maps:merge(Meta, Msg),
    {Data1, Acc} = format_syslog(Data0, Config, []),
    format_meta(Data1, Config, Acc).

-spec format_syslog(Data, Config, Acc) -> {Data, Acc} when
      Data :: map(),
      Config :: logger_formatter_config:config(),
      Acc :: list({atom() | unicode:chardata(), term()}).
format_syslog(Map = #{time := V}, C, Acc) ->
    format_syslog(maps:without([time], Map), C, [{"syslog.timestamp", format_time(V, C)} | Acc]);
format_syslog(Map = #{level := V}, C, Acc) ->
    format_syslog(maps:without([level], Map), C, [{"syslog.severity", V} | Acc]);
format_syslog(Map, _C, Acc) ->
    {Map, Acc}.

-spec format_meta(Data, Config, Acc) -> {Data, Acc} when
      Data :: map(),
      Config :: logger_formatter_config:config(),
      Acc :: list({atom() | unicode:chardata(), term()}).
format_meta(Meta0, _Config, Acc) ->
    {Meta1, Acc} = process_meta_map(Meta0, Acc),
    maps:fold(fun fold_meta/3, Acc, Meta1).

-spec fold_meta(K, V, Acc) -> Acc when
      K :: atom(),
      V :: term(),
      Acc :: list({atom() | unicode:chardata(), term()}).
fold_meta(file, V, Acc) ->
    [{"logger.file_name", V} | Acc];
fold_meta(line, V, Acc) ->
    [{"line", V} | Acc];
fold_meta(mfa, {M, F, A}, Acc) when is_atom(M), is_atom(F), is_list(A) ->
    fold_meta(mfa, {M, F, length(A)}, Acc);
fold_meta(mfa, {M, F, A}, Acc) when is_atom(M), is_atom(F), is_integer(A) ->
    [{"logger.method_name", io_lib:fwrite("~tw:~tw/~w", [M, F, A])} | Acc];
fold_meta(pid, V, Acc) when is_pid(V) ->
    [{"logger.thread_name", pid_to_list(V)} | Acc];
fold_meta(K, V, Acc) ->
    [{K, V} | Acc].

% Handle metadata which combines multiple metadata keys
process_meta_map(Map = #{module := Module, function := Function, arity := Arity}, Acc) ->
    process_meta_map(
      maps:without([module, function, arity], Map),
      [{"logger.method_name", [Module, ".", Function, "/", Arity]} | Acc]
     );

process_meta_map(Map = #{module := Module, function := Function}, Acc) ->
    process_meta_map(
      maps:without([module, function, arity], Map),
      [{"logger.method_name", [Module, ".", Function]} | Acc]
     );

process_meta_map(Map = #{function := Function}, Acc) ->
    process_meta_map(
      maps:without([module, function, arity], Map),
      [{"logger.method_name", [Function]} | Acc]
     );

process_meta_map(Map, Acc) ->
    {Map, Acc}.

% %% SysTime is the system time in microseconds
format_time(SysTime, #{time_offset := Offset, time_designator := Des})
  when is_integer(SysTime) ->
    calendar:system_time_to_rfc3339(SysTime,[{unit,microsecond},
                                             {offset,Offset},
                                             {time_designator,Des}]).

% to_string(X) when is_pid(X) ->
%     pid_to_list(X);
% to_string(X) when is_reference(X) ->
%     ref_to_list(X);
% to_string(X) when is_list(X) ->
%     case printable_list(lists:flatten(X)) of
%         true -> X;
%         _ -> io_lib:format(p(Config),[X])
%     end;
% to_string(X) ->
%     io_lib:format(p(Config),[X]).
