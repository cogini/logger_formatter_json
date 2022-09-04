%% @doc
%% This module is a formatter for the Erlang logger library which outputs JSON.
%%
%% https://www.erlang.org/doc/apps/kernel/logger_chapter.html#formatters
%% @end
%%
-module(logger_formatter_json).

-export([format/2]).
-export([check_config/1]).

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
                    template        => template(),
                    time_designator => byte(),
                    time_offset     => integer() | [byte()]}.
-type template() :: [metakey() | {metakey(),template(),template()} | unicode:chardata()].
-type metakey() :: atom() | [atom()].

% -type log_event() :: #{level:=level(),
%                        msg:={io:format(),[term()]} | {report,report()} | {string,unicode:chardata()},
%                        meta:=metadata()}.
%
% -type metadata() :: #{pid    => pid(),
%                       gl     => pid(),
%                       time   => timestamp(),
%                       mfa    => {module(),atom(),non_neg_integer()},
%                       file   => file:filename(),
%                       line   => non_neg_integer(),
%                       domain => [atom()],
%                       report_cb => report_cb(),
%                       atom() => term()}.

-define(IS_STRING(String),
        (is_list(String) orelse is_binary(String))).

%%%-----------------------------------------------------------------
%%% API
-spec format(LogEvent,Config) -> unicode:chardata() when
      LogEvent :: logger:log_event(),
      Config :: config().
format(#{level:=Level,msg:=Msg0,meta:=Meta},Config0)
  when is_map(Config0) ->
    Config = add_default_config(Config0),
    Template = maps:get(template,Config),
    {BT,AT0} = lists:splitwith(fun(msg) -> false; ({msg, _}) -> false; (_) -> true end, Template),
    {KeyOut,AT} =
        case AT0 of
            [{msg,KeyOut0}|Rest] ->
                {KeyOut0,Rest};
            [msg|Rest] ->
                {msg,Rest}
        end,
    MsgResult =
        case format_msg(Msg0,Meta,Config) of
            Msg when is_map(Msg) ->
                maps:to_list(Msg);
            Msg ->
                {KeyOut, iolist_to_binary(Msg)}
            end,
    Result0 = [
        do_format(Level,Meta,BT,Config),
        MsgResult,
        do_format(Level,Meta,AT,Config)
    ],
    Result = lists:flatten(Result0),
    [thoas:encode_to_iodata(Result), "\n"].


do_format(Level,Data,[Key|Format],Config) when is_atom(Key) ->
    do_format(Level,Data,[{Key,Key}|Format],Config);
do_format(Level,Data,[{Key,IfExist,Else}|Format],Config) when is_atom(Key) ->
    do_format(Level,Data,[{{Key,Key},IfExist,Else}|Format],Config);
do_format(Level,Data,[{level,KeyOut}|Format],Config) ->
    [{KeyOut,to_binary(level,Level,Config)}|do_format(Level,Data,Format,Config)];
do_format(Level,Data,[{{Key,KeyOut},IfExist,Else}|Format],Config) ->
    String0 =
        case value(Key,Data) of
            {ok,Value} -> do_format(Level,Data#{Key=>Value},IfExist,Config);
            error -> do_format(Level,Data,Else,Config)
        end,
    case String0 of
        [] ->
            do_format(Level,Data,Format,Config);
        String ->
            [{KeyOut,String}|do_format(Level,Data,Format,Config)]
    end;
do_format(Level,Data,[{Key,KeyOut}|Format],Config)
  when is_atom(Key) orelse
       (is_list(Key) andalso is_atom(hd(Key))) ->
    String0 =
        case value(Key,Data) of
            {ok,Value} -> to_binary(Key,Value,Config);
            error -> []
        end,
    case String0 of
        [] ->
            do_format(Level,Data,Format,Config);
        String ->
            [{KeyOut,String}|do_format(Level,Data,Format,Config)]
    end;
do_format(Level,Data,[Str|Format],Config) ->
    [Str|do_format(Level,Data,Format,Config)];
do_format(_Level,_Data,[],_Config) ->
    [].

value(Key,Meta) when is_map_key(Key,Meta) ->
    {ok,maps:get(Key,Meta)};
value([Key|Keys],Meta) when is_map_key(Key,Meta) ->
    value(Keys,maps:get(Key,Meta));
value([],Value) ->
    {ok,Value};
value(_,_) ->
    error.

to_binary(Key,Value,Config) ->
    iolist_to_binary(to_string(Key,Value,Config)).

to_string(time,Time,Config) ->
    format_time(Time,Config);
to_string(mfa,MFA,Config) ->
    format_mfa(MFA,Config);
to_string(initial_call,MFA,Config) ->
    format_mfa(MFA,Config);
% to_string(crash_reason,Value,Config) ->
%     format_crash_reason(Value,Config);
to_string(_,Value,Config) ->
    to_string(Value,Config).

to_string(X,_) when is_atom(X) ->
    atom_to_list(X);
to_string(X,_) when is_integer(X) ->
    integer_to_list(X);
to_string(X,_) when is_pid(X) ->
    pid_to_list(X);
to_string(X,_) when is_reference(X) ->
    ref_to_list(X);
to_string(X,Config) when is_list(X) ->
    case printable_list(lists:flatten(X)) of
        true -> X;
        _ -> io_lib:format(p(Config),[X])
    end;
to_string(X,Config) ->
    io_lib:format(p(Config),[X]).

printable_list([]) ->
    false;
printable_list(X) ->
    io_lib:printable_list(X).

format_msg({string,Chardata},Meta,Config) ->
    format_msg({"~ts",[Chardata]},Meta,Config);
format_msg({report,_}=Msg,Meta,#{report_cb:=Fun}=Config)
  when is_function(Fun,1); is_function(Fun,2) ->
    format_msg(Msg,Meta#{report_cb=>Fun},maps:remove(report_cb,Config));
format_msg({report,Report},#{report_cb:=Fun}=Meta,Config) when is_function(Fun,1) ->
    try Fun(Report) of
        {Format,Args} when is_list(Format), is_list(Args) ->
            format_msg({Format,Args},maps:remove(report_cb,Meta),Config);
        Other ->
            P = p(Config),
            format_msg({"REPORT_CB/1 ERROR: "++P++"; Returned: "++P,
                        [Report,Other]},Meta,Config)
    catch C:R:S ->
            P = p(Config),
            format_msg({"REPORT_CB/1 CRASH: "++P++"; Reason: "++P,
                        [Report,{C,R,logger:filter_stacktrace(?MODULE,S)}]},
                       Meta,Config)
    end;
format_msg({report,Report},#{report_cb:=Fun}=Meta,Config) when is_function(Fun,2) ->
    try Fun(Report,maps:with([depth,chars_limit,single_line],Config)) of
        Chardata when ?IS_STRING(Chardata) ->
            try chardata_to_list(Chardata) % already size limited by report_cb
            catch _:_ ->
                    P = p(Config),
                    format_msg({"REPORT_CB/2 ERROR: "++P++"; Returned: "++P,
                                [Report,Chardata]},Meta,Config)
            end;
        Other ->
            P = p(Config),
            format_msg({"REPORT_CB/2 ERROR: "++P++"; Returned: "++P,
                        [Report,Other]},Meta,Config)
    catch C:R:S ->
            P = p(Config),
            format_msg({"REPORT_CB/2 CRASH: "++P++"; Reason: "++P,
                        [Report,{C,R,logger:filter_stacktrace(?MODULE,S)}]},
                       Meta,Config)
    end;
% format_msg({report,#{label:={error_logger,_}, format:=Format, args:=Args},Meta,Config) ->
%     format_msg({Format, Args}, Meta, Config);
format_msg({report,Report},_Meta,_Config) when is_map(Report) ->
    Report;
format_msg({report,Report},Meta,Config) ->
    format_msg({report,Report},
               Meta#{report_cb=>fun logger:format_report/1},
               Config);
format_msg(Msg,_Meta,#{depth:=Depth,chars_limit:=CharsLimit,
                       single_line:=Single}) ->
    Opts = chars_limit_to_opts(CharsLimit),
    format_msg(Msg, Depth, Opts, Single).

chars_limit_to_opts(unlimited) -> [];
chars_limit_to_opts(CharsLimit) -> [{chars_limit,CharsLimit}].

format_msg({Format0,Args},Depth,Opts,Single) ->
    try
        Format1 = io_lib:scan_format(Format0, Args),
        Format = reformat(Format1, Depth, Single),
        io_lib:build_text(Format,Opts)
    catch C:R:S ->
            P = p(Single),
            FormatError = "FORMAT ERROR: "++P++" - "++P,
            case Format0 of
                FormatError ->
                    %% already been here - avoid failing cyclically
                    erlang:raise(C,R,S);
                _ ->
                    format_msg({FormatError,[Format0,Args]},Depth,Opts,Single)
            end
    end.

reformat(Format,unlimited,false) ->
    Format;
reformat([#{control_char:=C}=M|T], Depth, true) when C =:= $p ->
    [limit_depth(M#{width => 0}, Depth)|reformat(T, Depth, true)];
reformat([#{control_char:=C}=M|T], Depth, true) when C =:= $P ->
    [M#{width => 0}|reformat(T, Depth, true)];
reformat([#{control_char:=C}=M|T], Depth, Single) when C =:= $p; C =:= $w ->
    [limit_depth(M, Depth)|reformat(T, Depth, Single)];
reformat([H|T], Depth, Single) ->
    [H|reformat(T, Depth, Single)];
reformat([], _, _) ->
    [].

limit_depth(M0, unlimited) ->
    M0;
limit_depth(#{control_char:=C0, args:=Args}=M0, Depth) ->
    C = C0 - ($a - $A),				%To uppercase.
    M0#{control_char:=C,args:=Args++[Depth]}.

chardata_to_list(Chardata) ->
    case unicode:characters_to_list(Chardata,unicode) of
        List when is_list(List) ->
            List;
        Error ->
            throw(Error)
    end.

truncate(B,Msg,A,unlimited) ->
    [B,Msg,A];
truncate(B,Msg,A,Size) ->
    String = [B,Msg,A],
    Length = io_lib:chars_length(String),
    if Length>Size ->
            {Last,FlatString} =
                case A of
                    [] ->
                        case Msg of
                            [] ->
                                {get_last(B),lists:flatten(B)};
                            _ ->
                                {get_last(Msg),lists:flatten([B,Msg])}
                        end;
                    _ ->
                        {get_last(A),lists:flatten(String)}
                end,
            case Last of
                $\n->
                    lists:sublist(FlatString,1,Size-4)++"...\n";
                _ ->
                    lists:sublist(FlatString,1,Size-3)++"..."
            end;
       true ->
            String
    end.

get_last(L) ->
    get_first(lists:reverse(L)).

get_first([]) ->
    error;
get_first([C|_]) when is_integer(C) ->
    C;
get_first([L|Rest]) when is_list(L) ->
    case get_last(L) of
        error -> get_first(Rest);
        First -> First
    end.

%% SysTime is the system time in microseconds
format_time(SysTime,#{time_offset:=Offset,time_designator:=Des})
  when is_integer(SysTime) ->
    calendar:system_time_to_rfc3339(SysTime,[{unit,microsecond},
                                             {offset,Offset},
                                             {time_designator,Des}]).

format_mfa({M,F,A},_) when is_atom(M), is_atom(F), is_integer(A) ->
    io_lib:fwrite("~tw:~tw/~w", [M, F, A]);
format_mfa({M,F,A},Config) when is_atom(M), is_atom(F), is_list(A) ->
    format_mfa({M,F,length(A)},Config);
format_mfa(MFA,Config) ->
    to_string(MFA,Config).

% format_crash_reason({throw, Reason} ->
% format_crash_reason({exit, Reason} ->
% format_crash_reason({error, Exception, Stacktrace} ->

%% Ensure that all valid configuration parameters exist in the final
%% configuration map
-spec add_default_config(Config) -> logger:formatter_config() when
      Config :: logger:formatter_config().
add_default_config(Config0) ->
    Default =
        #{chars_limit=>unlimited,
          error_logger_notice_header=>info,
          legacy_header=>false,
          single_line=>true,
          time_designator=>$T},
    MaxSize = get_max_size(maps:get(max_size,Config0,undefined)),
    Depth = get_depth(maps:get(depth,Config0,undefined)),
    Offset = get_offset(maps:get(time_offset,Config0,undefined)),
    add_default_template(maps:merge(Default,Config0#{max_size=>MaxSize,
                                                     depth=>Depth,
                                                     time_offset=>Offset})).

add_default_template(#{template:=_}=Config) ->
    Config;
add_default_template(Config) ->
    Config#{template=>default_template(Config)}.

default_template(_) ->
    [
        {time, "syslog.timestamp"},
        % {level, "syslog.severity"},
        % https://docs.datadoghq.com/logs/log_configuration/processors/
        {level, "status"},
        {msg, message},
        % https://docs.datadoghq.com/logs/log_configuration/attributes_naming_convention/#source-code
        % error.kind	string	The error type or kind (or code in some cases).
        % error.message	string	A concise, human-readable, one-line message explaining the event.
        % error.stack	string	The stack trace or the complementary information about the error.
        {file, "logger.file_name"},
        line,
        {mfa, "logger.method_name"},
        {pid, "logger.thread_name"},
        {trace_id, "dd.trace_id"},
        {span_id, "dd.span_id"}

    ].

get_max_size(undefined) ->
    unlimited;
get_max_size(S) ->
    max(10,S).

get_depth(undefined) ->
    error_logger:get_format_depth();
get_depth(S) ->
    max(5,S).

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

-spec check_config(Config) -> ok | {error,term()} when
      Config :: config().
check_config(Config) when is_map(Config) ->
    do_check_config(maps:to_list(Config));
check_config(Config) ->
    {error,{invalid_formatter_config,?MODULE,Config}}.

do_check_config([{Type,L}|Config]) when Type == chars_limit;
                                        Type == depth;
                                        Type == max_size ->
    case check_limit(L) of
        ok -> do_check_config(Config);
        error -> {error,{invalid_formatter_config,?MODULE,{Type,L}}}
    end;
do_check_config([{single_line,SL}|Config]) when is_boolean(SL) ->
    do_check_config(Config);
do_check_config([{legacy_header,LH}|Config]) when is_boolean(LH) ->
    do_check_config(Config);
do_check_config([{error_logger_notice_header,ELNH}|Config]) when ELNH == info;
                                                                 ELNH == notice ->
    do_check_config(Config);
do_check_config([{report_cb,RCB}|Config]) when is_function(RCB,1);
                                               is_function(RCB,2) ->
    do_check_config(Config);
do_check_config([{template,T}|Config]) ->
    case check_template(T) of
        ok -> do_check_config(Config);
        error -> {error,{invalid_formatter_template,?MODULE,T}}
    end;
do_check_config([{time_offset,Offset}|Config]) ->
    case check_offset(Offset) of
        ok ->
            do_check_config(Config);
        error ->
            {error,{invalid_formatter_config,?MODULE,{time_offset,Offset}}}
    end;
do_check_config([{time_designator,Char}|Config]) when Char>=0, Char=<255 ->
    case io_lib:printable_latin1_list([Char]) of
        true ->
            do_check_config(Config);
        false ->
            {error,{invalid_formatter_config,?MODULE,{time_designator,Char}}}
    end;
do_check_config([C|_]) ->
    {error,{invalid_formatter_config,?MODULE,C}};
do_check_config([]) ->
    ok.

check_limit(L) when is_integer(L), L>0 ->
    ok;
check_limit(unlimited) ->
    ok;
check_limit(_) ->
    error.

check_template([Key|T]) when is_atom(Key) ->
    check_template(T);
check_template([{Key,_KeyOut}|T]) when is_atom(Key) ->
    check_template(T);
check_template([Key|T]) when is_list(Key), is_atom(hd(Key)) ->
    case lists:all(fun(X) when is_atom(X) -> true;
                      (_) -> false
                   end,
                   Key) of
        true ->
            check_template(T);
        false ->
            error
    end;
check_template([{{Key,KeyOut},IfExist,Else}|T])
  when is_atom(Key) orelse
       (is_list(Key) andalso is_atom(hd(Key))) ->
    check_template([{Key,IfExist,Else}|T]);
check_template([{Key,IfExist,Else}|T])
  when is_atom(Key) orelse
       (is_list(Key) andalso is_atom(hd(Key))) ->
    case check_template(IfExist) of
        ok ->
            case check_template(Else) of
                ok ->
                    check_template(T);
                error ->
                    error
            end;
        error ->
            error
    end;
check_template([Str|T]) when is_list(Str) ->
    case io_lib:printable_unicode_list(Str) of
        true -> check_template(T);
        false -> error
    end;
check_template([Bin|T]) when is_binary(Bin) ->
    case unicode:characters_to_list(Bin) of
        Str when is_list(Str) -> check_template([Str|T]);
        _Error -> error
    end;
check_template([]) ->
    ok;
check_template(_) ->
    error.

check_offset(I) when is_integer(I) ->
    ok;
check_offset(Tz) when Tz=:=""; Tz=:="Z"; Tz=:="z" ->
    ok;
check_offset([Sign|Tz]) when Sign=:=$+; Sign=:=$- ->
    check_timezone(Tz);
check_offset(_) ->
    error.

check_timezone(Tz) ->
    try io_lib:fread("~d:~d", Tz) of
        {ok, [_, _], []} ->
            ok;
        _ ->
            error
    catch _:_ ->
            error
    end.

p(#{single_line:=Single}) ->
    p(Single);
p(true) ->
    "~0tp";
p(false) ->
    "~tp".
