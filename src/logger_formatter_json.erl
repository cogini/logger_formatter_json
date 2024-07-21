%% @doc
%% Formatter for the Erlang logger library which outputs JSON.
%% https://www.erlang.org/doc/apps/kernel/logger_chapter.html#formatters
%%
%% @end
%%

-module(logger_formatter_json).

-export([format/2]).
-export([check_config/1]).

-ifdef(TEST).

-export([format_msg/3, to_string/2, is_printable/1, printable_list/1, print_string/2]).

-endif.

%%%-----------------------------------------------------------------
%%% Types

-type config() :: #{
                  chars_limit => pos_integer() | unlimited,
                  depth => pos_integer() | unlimited,
                  map_msg => atom(),
                  max_size => pos_integer() | unlimited,
                  names => map() | atom() | [atom() | map()],
                  types => map() | [map()],
                  report_cb => logger:report_cb(),
                  single_line => boolean(),
                  template => template(),
                  time_designator => byte(),
                  time_offset => integer() | [byte()]
                }.
-type template() :: [
                    metakey()
                    | {metakey(), template(), template()}
                    | {group, metakey(), atom(), [atom()]}
                    | {group, metakey(), atom(), [atom()], map()}
                  ].
-type metakey() :: atom() | [atom()].

-define(IS_STRING(String), is_list(String) orelse is_binary(String)).

%%%-----------------------------------------------------------------
%%% API

-spec format(LogEvent, Config) ->
  unicode:chardata() when LogEvent :: logger:log_event(), Config :: config().
format(#{level := Level, msg := {report, V} = _Msg, meta := Meta}, #{map_msg := merge} = Config0)
when is_map(V) ->
  Config = add_default_config(Config0),
  Template0 = maps:get(template, Config),
  Template = lists:dropwhile(fun (msg) -> true; (_) -> false end, Template0),
  Result0 = [do_format(Level, maps:merge(Meta, V), Template, [], Config)],
  Result = lists:flatten(Result0),
  [thoas:encode_to_iodata(Result, #{escape => unicode}), "\n"];

format(#{level := Level, msg := Msg, meta := Meta}, Config0) when is_map(Config0) ->
  Config = add_default_config(Config0),
  Template = maps:get(template, Config),
  Result0 =
    [do_format(Level, maps:put(msg, format_msg(Msg, Meta, Config), Meta), Template, [], Config)],
  Result = lists:flatten(Result0),
  [thoas:encode_to_iodata(Result, #{escape => unicode}), "\n"].


% @doc Map metadata key to JSON output name
-spec map_name(Key, Config) -> atom() | binary() when Key :: atom(), Config :: config().
map_name(Key, #{names := Names}) -> maps:get(Key, Names, Key).

% @doc Map metadata value to a different type
-spec map_type(Key, Config) -> atom() | {atom(), atom()} when Key :: atom(), Config :: config().
map_type(Key, #{types := Types}) -> maps:get(Key, Types, Key).

-spec do_format(Level, Data, Template, Seen, Config) ->
  [{atom() | binary(), binary()}]
  when Level :: atom(),
       Data :: map(),
       Template :: template(),
       Seen :: [atom() | [atom()]],
       Config :: config().
do_format(Level, Data0, [all | _Format], _Seen, Config) ->
  Data = maps:put(level, Level, Data0),
  lists:map(fun ({K, V}) -> {map_name(K, Config), to_output(K, V, Config)} end, maps:to_list(Data));

do_format(Level, Data0, [rest | _Format], Seen, Config) ->
  Data1 = maps:put(level, Level, Data0),
  Data = maps:without(lists:flatten(Seen), Data1),
  lists:map(fun ({K, V}) -> {map_name(K, Config), to_output(K, V, Config)} end, maps:to_list(Data));

do_format(Level, Data0, [{group, Key, Keys} | Format], Seen, Config) ->
  do_format(Level, Data0, [{group, Key, Keys, #{}} | Format], Seen, Config);

do_format(Level, Data0, [{group, Key, Keys, GroupTypes} | Format], Seen, Config) ->
  Types = maps:merge(maps:get(types, Config), GroupTypes),
  Data = maps:with(Keys, Data0),
  Data1 =
    lists:map(
      fun ({K, V}) -> {maps:get(K, Types, K), to_output(K, V, Config)} end,
      maps:to_list(Data)
    ),
  [{map_name(Key, Config), Data1} | do_format(Level, Data0, Format, [Keys | Seen], Config)];

do_format(Level, Data, [level | Format], Seen, Config) ->
  [
    {map_name(level, Config), to_output(map_type(level, Config), Level, Config)}
    | do_format(Level, Data, Format, [level | Seen], Config)
  ];

do_format(Level, Data, [{Key, IfExist, Else} | Format], Seen, Config) ->
  String0 =
    case value(Key, Data) of
      {ok, Value} -> do_format(Level, Data#{Key => Value}, IfExist, Seen, Config);
      error -> do_format(Level, Data, Else, [Key | Seen], Config)
    end,
  case String0 of
    [] -> do_format(Level, Data, Format, Seen, Config);
    String -> [{map_name(Key, Config), String} | do_format(Level, Data, Format, Seen, Config)]
  end;

do_format(Level, Data, [Key | Format], Seen, Config)
when is_atom(Key) orelse is_list(Key) andalso is_atom(hd(Key)) ->
  String0 =
    case value(Key, Data) of
      {ok, Value} -> to_output(map_type(Key, Config), Value, Config);
      error -> []
    end,
  case String0 of
    [] -> do_format(Level, Data, Format, [Key | Seen], Config);

    String ->
      [{map_name(Key, Config), String} | do_format(Level, Data, Format, [Key | Seen], Config)]
  end;

% do_format(Level,Data,[Str|Format],Config) ->
%     [Str|do_format(Level,Data,Format,Config)];
do_format(_Level, _Data, [], _Seen, _Config) -> [].


value(Key, Meta) when is_map_key(Key, Meta) -> {ok, maps:get(Key, Meta)};
value([Key | Keys], Meta) when is_map_key(Key, Meta) -> value(Keys, maps:get(Key, Meta));
value([], Value) -> {ok, Value};
value(_, _) -> error.

to_output(_Key, Value, _Config) when is_map(Value) -> maps:to_list(Value);
to_output(Key, Value, Config) -> iolist_to_binary(to_string(Key, Value, Config)).

to_string({level, OutputFormat}, Value, Config) -> format_level(OutputFormat, Value, Config);
to_string(system_time, Value, Config) -> format_time(Value, Config);
% to_string({system_time, OutputFormat},Value,Config) ->
%     format_time(OutputFormat, Value,Config);
to_string(mfa, Value, Config) -> format_mfa(Value, Config);
% to_string(crash_reason,Value,Config) ->
%     format_crash_reason(Value,Config);
to_string(_, Value, Config) -> to_string(Value, Config).

to_string(X, _) when is_atom(X) -> atom_to_list(X);
to_string(X, _) when is_integer(X) -> integer_to_list(X);
to_string(X, _) when is_pid(X) -> pid_to_list(X);
to_string(X, _) when is_reference(X) -> ref_to_list(X);

to_string(X, Config) when is_list(X) ->
  case printable_list(lists:flatten(X)) of
    true -> X;
    _ -> io_lib:format(p(Config), [X])
  end;

to_string(<<>>, _Config) -> <<>>;

to_string(X, Config) when is_binary(X) ->
  case is_printable(X) of
    true -> X;
    _ -> io_lib:format(p(Config), [X])
  end;

to_string(X, Config) -> io_lib:format(p(Config), [X]).


-spec process_io_list(List, Config) -> binary() when List :: list(), Config :: config().
% process_io_list(List, Config) when is_list(List) ->
%     Strings = lists:map(fun (X) -> iolist_to_string(X, Config) end, List),
%     iolist_to_binary(Strings).
process_io_list([], _Config) -> [];
process_io_list(List, Config) -> process_io_list(List, [], Config).

process_io_list([], Acc, _Config) -> iolist_to_binary(lists:reverse(Acc));

process_io_list([H | T], Acc, Config) when is_list(H) ->
  process_io_list(T, [process_io_list(H, [], Config) | Acc], Config);

process_io_list([H | T], Acc, Config) ->
  process_io_list(T, [iolist_to_string(H, Config) | Acc], Config);

% Handle improper list, e.g., [<<"foo">> | <<"bar">>
% Crash reports can contain improper lists
process_io_list(T, Acc, Config) -> process_io_list([], [iolist_to_string(T, Config) | Acc], Config).

% @doc Format embedded things in iolist that iolist_to_binary chokes on
% https://www.erlang.org/doc/system/expressions.html
% is_atom/1
% is_binary/1
% is_bitstring/1
% is_boolean/1
% is_float/1
% is_function/1
% is_function/2
% is_integer/1
% is_list/1
% is_map/1
% is_number/1
% is_pid/1
% is_port/1
% is_record/2
% is_record/3
% is_reference/1
% is_tuple/1
iolist_to_string(X, _) when is_atom(X) -> atom_to_list(X);
iolist_to_string(X, _) when is_integer(X) -> X;
iolist_to_string(X, _) when is_pid(X) -> pid_to_list(X);
iolist_to_string(X, _) when is_reference(X) -> ref_to_list(X);
iolist_to_string(<<>>, _) -> <<>>;

iolist_to_string(X, Config) when is_binary(X) ->
  case is_printable(X) of
    true -> X;
    _ -> io_lib:format(p(Config), [X])
  end;

iolist_to_string(X, Config) when is_list(X) ->
  % case printable_list(lists:flatten(X)) of
  case printable_list(X) of
    true -> X;
    _ -> io_lib:format(p(Config), [X])
  end;

iolist_to_string(X, Config) -> io_lib:format(p(Config), [X]).


-spec printable_list(list()) -> boolean().
% Print empty string as empty list
printable_list([]) -> false;
printable_list(X) -> io_lib:printable_unicode_list(X).

-spec is_printable(term()) -> boolean().
is_printable(X) when is_binary(X) ->
  % case re:run(X, <<"[[:^print:]]">>, [{capture, none}, unicode]) of
  %   match -> false;
  %   _ -> true
  % end;
  io_lib:printable_unicode_list(unicode:characters_to_list(X, unicode)).


% Format strings for print
print_string(X, Config) when is_binary(X) ->
  case is_printable(X) of
    true -> X;
    _ -> io_lib:format(p(Config), [X])
  end;

print_string(X, Config) when is_list(X) ->
  case printable_list(lists:flatten(X)) of
    true -> list_to_binary(X);
    _ -> io_lib:format(p(Config), [X])
  end.


-spec format_msg(Msg, Meta, Config) ->
  binary()
  | map() when Msg :: {io:format(), [term()]}
  | {report, logger:report()}
  | {string, unicode:chardata()} , Meta :: logger:metadata() , Config :: config().
% format_msg({string, Chardata}, Meta, Config) -> format_msg({"~ts", [Chardata]}, Meta, Config);
format_msg({string, Chardata}, Meta, Config) when is_binary(Chardata) ->
  case is_printable(Chardata) of
    true -> format_msg({"~ts", [Chardata]}, Meta, Config);
    false -> format_msg({"~tp", [Chardata]}, Meta, Config)
  end;

format_msg({string, Chardata}, Meta, Config) ->
  case io_lib:printable_unicode_list(Chardata) of
    true -> format_msg({"~ts", [Chardata]}, Meta, Config);
    false -> try format_msg({"~ts", [process_io_list(Chardata, Config)]}, Meta, Config) catch
        % _:_ ->
        %   format_msg({"~tp", [Chardata]}, Meta, Config)
        C:R : S ->
          P = p(Config),
          format_msg(
            {
              "FORMAT CRASH: " ++ P ++ "; Reason: " ++ P,
              % [Chardata, {C, R, logger:filter_stacktrace(?MODULE, S)}]
              [Chardata, {C, R, S}]
            },
            Meta,
            Config
          ) end
  end;

format_msg({report, Report}, _Meta, Config) when is_map(Report) ->
  maps:map(fun (_K, V) -> print_string(to_string(V, Config), Config) end, Report);

format_msg({report, _} = Msg, Meta, #{report_cb := Fun} = Config)
when is_function(Fun, 1); is_function(Fun, 2) ->
  format_msg(Msg, Meta#{report_cb => Fun}, maps:remove(report_cb, Config));

format_msg({report, Report}, #{report_cb := Fun} = Meta, Config) when is_function(Fun, 1) ->
  try Fun(Report) of
    {Format, Args} when is_list(Format), is_list(Args) ->
      format_msg({Format, Args}, maps:remove(report_cb, Meta), Config);

    Other ->
      P = p(Config),
      format_msg({"REPORT_CB/1 ERROR: " ++ P ++ "; Returned: " ++ P, [Report, Other]}, Meta, Config)
  catch
    C:R : S ->
      P = p(Config),
      format_msg(
        {
          "REPORT_CB/1 CRASH: " ++ P ++ "; Reason: " ++ P,
          [Report, {C, R, logger:filter_stacktrace(?MODULE, S)}]
        },
        Meta,
        Config
      )
  end;

format_msg({report, Report}, #{report_cb := Fun} = Meta, Config) when is_function(Fun, 2) ->
  try Fun(Report, maps:with([depth, chars_limit, single_line], Config)) of
    Chardata when ?IS_STRING(Chardata) ->
      try
        % already size limited by report_cb
        chardata_to_list(Chardata)
      catch
        _:_ ->
          P = p(Config),
          format_msg(
            {"REPORT_CB/2 ERROR: " ++ P ++ "; Returned: " ++ P, [Report, Chardata]},
            Meta,
            Config
          )
      end;

    Other ->
      P = p(Config),
      format_msg({"REPORT_CB/2 ERROR: " ++ P ++ "; Returned: " ++ P, [Report, Other]}, Meta, Config)
  catch
    C:R : S ->
      P = p(Config),
      format_msg(
        {
          "REPORT_CB/2 CRASH: " ++ P ++ "; Reason: " ++ P,
          [Report, {C, R, logger:filter_stacktrace(?MODULE, S)}]
        },
        Meta,
        Config
      )
  end;

% format_msg({report,#{label:={error_logger,_}, format:=Format, args:=Args},Meta,Config) ->
%     format_msg({Format, Args}, Meta, Config);
format_msg({report, Report}, Meta, Config) ->
  format_msg({report, Report}, Meta#{report_cb => fun logger:format_report/1}, Config);

format_msg(Msg, _Meta, #{depth := Depth, chars_limit := CharsLimit, single_line := Single}) ->
  Opts = chars_limit_to_opts(CharsLimit),
  format_msg(Msg, Depth, Opts, Single).


-spec chars_limit_to_opts(CharsLimit :: unlimited | non_neg_integer()) -> proplists:proplist().
chars_limit_to_opts(unlimited) -> [];
chars_limit_to_opts(CharsLimit) -> [{chars_limit, CharsLimit}].

format_msg({Format0, Args}, Depth, Opts, Single) ->
  try
    Format1 = io_lib:scan_format(Format0, Args),
    Format = reformat(Format1, Depth, Single),
    BuildText = io_lib:build_text(Format, Opts),
    unicode:characters_to_binary(BuildText)
  catch
    C:R : S ->
      P = p(Single),
      FormatError = "FORMAT ERROR: " ++ P ++ " - " ++ P,
      case Format0 of
        FormatError ->
          %% already been here - avoid failing cyclically
          erlang:raise(C, R, S);

        _ -> format_msg({FormatError, [Format0, Args]}, Depth, Opts, Single)
      end
  end.


reformat(Format, unlimited, false) -> Format;

reformat([#{control_char := C} = M | T], Depth, true) when C =:= $p ->
  [limit_depth(M#{width => 0}, Depth) | reformat(T, Depth, true)];

reformat([#{control_char := C} = M | T], Depth, true) when C =:= $P ->
  [M#{width => 0} | reformat(T, Depth, true)];

reformat([#{control_char := C} = M | T], Depth, Single) when C =:= $p; C =:= $w ->
  [limit_depth(M, Depth) | reformat(T, Depth, Single)];

reformat([H | T], Depth, Single) -> [H | reformat(T, Depth, Single)];
reformat([], _, _) -> [].

limit_depth(M0, unlimited) -> M0;

limit_depth(#{control_char := C0, args := Args} = M0, Depth) ->
  %To uppercase.
  C = C0 - ($a - $A),
  M0#{control_char := C, args := Args ++ [Depth]}.


chardata_to_list(Chardata) ->
  case unicode:characters_to_list(Chardata, unicode) of
    List when is_list(List) -> List;
    Error -> throw(Error)
  end.


% https://cloud.google.com/logging/docs/reference/v2/rest/v2/LogEntry#LogSeverity
format_level(gcp, emergency, _Config) -> <<"EMERGENCY">>;
format_level(gcp, alert, _Config) -> <<"ALERT">>;
format_level(gcp, critical, _Config) -> <<"CRITICAL">>;
format_level(gcp, error, _Config) -> <<"ERROR">>;
format_level(gcp, warning, _Config) -> <<"WARNING">>;
format_level(gcp, notice, _Config) -> <<"INFO">>;
format_level(gcp, info, _Config) -> <<"INFO">>;
format_level(gcp, debug, _Config) -> <<"DEBUG">>;
format_level(gcp, _, _Config) -> <<"DEFAULT">>.

%% SysTime is the system time in microseconds

format_time(SysTime, Config) when is_integer(SysTime) ->
  #{time_offset := Offset, time_designator := Des} = Config,
  calendar:system_time_to_rfc3339(
    SysTime,
    [{unit, microsecond}, {offset, Offset}, {time_designator, Des}]
  ).


format_mfa({M, F, A}, _) when is_atom(M), is_atom(F), is_integer(A) ->
  io_lib:fwrite("~tw:~tw/~w", [M, F, A]);

format_mfa({M, F, A}, Config) when is_atom(M), is_atom(F), is_list(A) ->
  format_mfa({M, F, length(A)}, Config);

format_mfa(MFA, Config) -> to_string(MFA, Config).

% format_crash_reason({throw, Reason} ->
% format_crash_reason({exit, Reason} ->
% format_crash_reason({error, Exception, Stacktrace} ->

%% Ensure that all valid configuration parameters exist in the final
%% configuration map

-spec add_default_config(Config) -> config() when Config :: logger:formatter_config().
add_default_config(Config0) ->
  Default =
    #{
      chars_limit => unlimited,
      % error_logger_notice_header => info,
      % legacy_header => false,
      single_line => true,
      time_designator => $T
    },
  MaxSize = get_max_size(maps:get(max_size, Config0, undefined)),
  Depth = get_depth(maps:get(depth, Config0, undefined)),
  Offset = get_offset(maps:get(time_offset, Config0, undefined)),
  Names = get_names(maps:get(names, Config0, undefined)),
  Types = get_types(maps:get(types, Config0, undefined)),
  Template = expand_templates(maps:get(template, Config0, [{keys, all}])),
  MapMsg = maps:get(map_msg, Config0, merge),
  maps:merge(
    Default,
    Config0#{
      max_size => MaxSize,
      depth => Depth,
      names => Names,
      types => Types,
      template => Template,
      time_offset => Offset,
      map_msg => MapMsg
    }
  ).


expand_templates(Templates0) ->
  Templates1 = lists:map(fun default_template/1, Templates0),
  Templates2 = remove_dups(Templates1),
  lists:flatten(Templates2).


default_template({keys, all}) -> [msg, rest];
default_template({keys, basic}) -> [time, level, msg];
default_template({keys, trace}) -> [trace_id, span_id];
default_template({keys, otel}) -> [otel_trace_id, otel_span_id, otel_trace_flags];

default_template({keys, gcp}) ->
  [
    msg,
    time,
    level,
    trace_id,
    span_id,
    {group, source_location, [file, line, mfa]},
    {group, tags, [rest]}
  ];

default_template(Value) -> Value.

get_max_size(undefined) -> unlimited;
get_max_size(S) -> max(10, S).

get_depth(undefined) -> error_logger:get_format_depth();
get_depth(S) -> max(5, S).

-spec get_names(Names) -> map() when Names :: atom() | map() | [atom() | map()].
get_names(Names) when is_list(Names) ->
  lists:foldl(fun (M, Acc) -> maps:merge(Acc, default_names(M)) end, #{}, Names);

get_names(Names) -> default_names(Names).

-spec default_names(Names) -> map() when Names :: atom() | map().
default_names(Names) when is_map(Names) -> Names;

default_names(datadog) ->
  % https://docs.datadoghq.com/logs/log_configuration/processors/
  % https://docs.datadoghq.com/logs/log_configuration/attributes_naming_convention/#source-code
  % https://docs.datadoghq.com/tracing/faq/why-cant-i-see-my-correlated-logs-in-the-trace-id-panel/?tab=jsonlogs
  #{
    time => <<"date">>,
    level => <<"status">>,
    msg => <<"message">>,
    trace_id => <<"dd.trace_id">>,
    span_id => <<"dd.span_id">>,
    % level => <<"syslog.severity">>,
    % time => <<"syslog.timestamp">>,
    file => <<"logger.file_name">>,
    mfa => <<"logger.method_name">>,
    pid => <<"logger.thread_name">>
  };

% error.kind	string	The error type or kind (or code in some cases).
% error.message	string	A concise, human-readable, one-line message explaining the event.
% error.stack	string	The stack trace or the complementary information about the error.
default_names(gcp) ->
  % https://cloud.google.com/logging/docs/reference/v2/rest/v2/LogEntry
  % TODO: this is incomplete
  #{
    time => <<"timestamp">>,
    level => <<"severity">>,
    msg => <<"textPayload">>,
    trace_id => <<"trace">>,
    span_id => <<"spanId">>,
    source_location => <<"sourceLocation">>,
    [source_location, file] => <<"file">>,
    [source_location, line] => <<"line">>,
    [source_location, mfa] => <<"function">>
  };

default_names(_) -> #{}.


-spec get_types(Names) -> map() when Names :: atom() | map() | [atom() | map()].
get_types(Types) when is_list(Types) ->
  Defaults = #{time => system_time, level => level, mfa => mfa, initial_call => mfa},
  lists:foldl(fun (M, Acc) -> maps:merge(Acc, default_types(M)) end, Defaults, Types);

get_types(Types) -> default_types(Types).


-spec default_types(Types) -> map() when Types :: atom() | map().
default_types(Types) when is_map(Types) -> Types;
default_types(undefined) -> #{time => system_time, level => level, mfa => mfa, initial_call => mfa};

default_types(gcp) ->
  % https://cloud.google.com/logging/docs/reference/v2/rest/v2/LogEntry#LogSeverity
  #{level => {level, gcp}};

default_types(_) -> #{}.


get_offset(undefined) -> utc_to_offset(get_utc_config());
get_offset(Offset) -> Offset.

utc_to_offset(true) -> "Z";
utc_to_offset(false) -> "".

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


-spec check_config(Config) -> ok | {error, term()} when Config :: config().
check_config(Config) when is_map(Config) -> do_check_config(maps:to_list(Config));
check_config(Config) -> {error, {invalid_formatter_config, ?MODULE, Config}}.

do_check_config([{Type, L} | Config]) when Type == chars_limit; Type == depth; Type == max_size ->
  case check_limit(L) of
    ok -> do_check_config(Config);
    error -> {error, {invalid_formatter_config, ?MODULE, {Type, L}}}
  end;

do_check_config([{map_msg, embed} | Config]) -> do_check_config(Config);
do_check_config([{map_msg, merge} | Config]) -> do_check_config(Config);

do_check_config([{map_msg, Format} | _Config]) ->
  {error, {invalid_formatter_config, ?MODULE, {map_msg, Format}}};

do_check_config([{single_line, SL} | Config]) when is_boolean(SL) -> do_check_config(Config);
do_check_config([{legacy_header, LH} | Config]) when is_boolean(LH) -> do_check_config(Config);
% do_check_config([{error_logger_notice_header, ELNH} | Config])
%     when ELNH == info; ELNH == notice ->
%     do_check_config(Config);
do_check_config([{report_cb, RCB} | Config]) when is_function(RCB, 1); is_function(RCB, 2) ->
  do_check_config(Config);

do_check_config([{template, T} | Config]) ->
  case check_template(T) of
    ok -> do_check_config(Config);
    error -> {error, {invalid_formatter_template, ?MODULE, T}}
  end;

do_check_config([{time_offset, Offset} | Config]) ->
  case check_offset(Offset) of
    ok -> do_check_config(Config);
    error -> {error, {invalid_formatter_config, ?MODULE, {time_offset, Offset}}}
  end;

do_check_config([{names, Names} | Config]) ->
  case check_names(Names) of
    ok -> do_check_config(Config);
    error -> {error, {invalid_formatter_config, ?MODULE, {names, Names}}}
  end;

do_check_config([{types, Names} | Config]) ->
  case check_types(Names) of
    ok -> do_check_config(Config);
    error -> {error, {invalid_formatter_config, ?MODULE, {types, Names}}}
  end;

do_check_config([{time_designator, Char} | Config]) when Char >= 0, Char =< 255 ->
  case io_lib:printable_latin1_list([Char]) of
    true -> do_check_config(Config);
    false -> {error, {invalid_formatter_config, ?MODULE, {time_designator, Char}}}
  end;

do_check_config([C | _]) -> {error, {invalid_formatter_config, ?MODULE, C}};
do_check_config([]) -> ok.


check_limit(L) when is_integer(L), L > 0 -> ok;
check_limit(unlimited) -> ok;
check_limit(_) -> error.

check_template([Key | T]) when is_atom(Key) -> check_template(T);

check_template([Key | T]) when is_list(Key), is_atom(hd(Key)) ->
  case lists:all(fun (X) when is_atom(X) -> true; (_) -> false end, Key) of
    true -> check_template(T);
    false -> error
  end;

check_template([{Key, IfExist, Else} | T])
when is_atom(Key) orelse is_list(Key) andalso is_atom(hd(Key)) ->
  case check_template(IfExist) of
    ok ->
      case check_template(Else) of
        ok -> check_template(T);
        error -> error
      end;

    error -> error
  end;

check_template([Str | T]) when is_list(Str) ->
  case io_lib:printable_unicode_list(Str) of
    true -> check_template(T);
    false -> error
  end;

check_template([Bin | T]) when is_binary(Bin) ->
  case unicode:characters_to_list(Bin) of
    Str when is_list(Str) -> check_template([Str | T]);
    _Error -> error
  end;

check_template([]) -> ok;
check_template(_) -> error.


check_names(datadog) -> ok;
check_names(gcp) -> ok;
check_names(Names) when is_atom(Names) -> error;
check_names(Names) when is_map(Names) -> ok;

check_names(Names) when is_list(Names) ->
  case lists:all(fun (N) -> check_names(N) == ok end, Names) of
    true -> ok;
    false -> error
  end;

check_names(_) -> error.


check_types(gcp) -> ok;
check_types(Types) when is_atom(Types) -> error;
check_types(Types) when is_map(Types) -> ok;

check_types(Types) when is_list(Types) ->
  case lists:all(fun (N) -> check_types(N) == ok end, Types) of
    true -> ok;
    false -> error
  end;

check_types(_) -> error.


check_offset(I) when is_integer(I) -> ok;
check_offset(Tz) when Tz =:= ""; Tz =:= "Z"; Tz =:= "z" -> ok;
check_offset([Sign | Tz]) when Sign =:= $+; Sign =:= $- -> check_timezone(Tz);
check_offset(_) -> error.

check_timezone(Tz) ->
  try io_lib:fread("~d:~d", Tz) of
    {ok, [_, _], []} -> ok;
    _ -> error
  catch
    _:_ -> error
  end.


p(#{single_line := Single}) -> p(Single);
p(true) -> "~0tp";
p(false) -> "~tp".

remove_dups([]) -> [];
remove_dups([H | T]) -> [H | [X || X <- remove_dups(T), X /= H]].
