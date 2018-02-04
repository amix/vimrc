#!/usr/bin/env escript

main([File]) ->
    Dir = get_root(filename:dirname(File)),
    Defs = [strong_validation,
            warn_export_all,
            warn_export_vars,
            warn_shadow_vars,
            warn_obsolete_guard,
            warn_unused_import,
            report,
            {i, Dir ++ "/include"}],
    %% `rebar.config` is looked for,
    %% but it is not necessarily the one in the project root.
    %% I.e. it may be one deeper in the project file hierarchy.
    Profile = which_compile_opts_profile(filename:absname(File)),
    CompileOpts = case which_build_tool(Dir, Profile) of
        {rebar, RebarFile} ->
            %% `rebar.config` might contain relative paths.
            %% They are relative to the file! Not to the project root.
            %% rebar specific begin
            rebar_opts(RebarFile);
            %% rebar specific end
        {erlangmk, ErlangMkDir} ->
            %% Erlang.mk specific begin
            erlangmk_opts(ErlangMkDir, Profile);
            %% Erlang.mk specific end
        undefined ->
            fallback_opts()
    end,
    code:add_patha(filename:absname("ebin")),
    %% `compile:file/2` requires the `{i, Path}` to be relative
    %% to CWD - no surprise here.
    compile:file(File, Defs ++ translate_paths(Dir, CompileOpts));

main(_) ->
    io:format("Usage: ~s <file>~n", [escript:script_name()]),
    halt(1).

which_compile_opts_profile(File) ->
    case filename:basename(filename:dirname(File)) of
        "test" -> test;
        _      -> normal
    end.

which_build_tool(Dir, Profile) ->
    %% rebar specific begin
    RebarFile = rebar_file(Dir, Profile),
    %% rebar specific end
    case filelib:is_file(RebarFile) of
        true ->
            {rebar, RebarFile};
        false ->
            %% Erlang.mk specific begin
            ErlangMk = erlangmk_file(Dir),
            %% Erlang.mk specific end
            case filelib:is_file(ErlangMk) of
                true  -> {erlangmk, Dir};
                false -> undefined
            end
    end.

rebar_file(Dir, normal) -> filename:join(Dir, "rebar.config");
rebar_file(Dir, test)   -> filename:join(Dir, "rebar.test.config").

erlangmk_file(Dir) -> filename:join(Dir, "erlang.mk").

rebar_opts(RebarFile) ->
    Dir = get_root(filename:dirname(RebarFile)),
    case file:consult(RebarFile) of
        {ok, Terms} ->
            %% Add deps for a rebar (version < 3) project
            RebarLibDirs = proplists:get_value(lib_dirs, Terms, []),
            lists:foreach(
                fun(LibDir) ->
                        code:add_pathsa(filelib:wildcard(LibDir ++ "/*/ebin"))
                end, RebarLibDirs),
            RebarDepsDir = proplists:get_value(deps_dir, Terms, "deps"),
            code:add_pathsa(filelib:wildcard(RebarDepsDir ++ "/*/ebin")),

            %% Add deps for rebar 3
            code:add_pathsa(filelib:wildcard(Dir ++ "/_build/default/lib/*/ebin")),
            %% Add include dependencies
            IncludeDeps = [{i, IPath} || IPath <- filelib:wildcard(Dir ++ "/_build/default/lib/*")] ++
                            [{i, filename:join(Dir, RebarDepsDir)}, %% rebar 2 dependencies
                             {i, filename:join(Dir, "apps")}], %% rebar 3 multi-apps
            proplists:get_value(erl_opts, Terms, []) ++ IncludeDeps;
        {error, _} when RebarFile == "rebar.config" ->
          fallback_opts();
        {error, _} ->
            rebar_opts("rebar.config")
    end.

erlangmk_opts(BaseDir, Profile) ->
    Make =
        case os:getenv("MAKE") of
            false ->
                case os:find_executable("gmake") of
                    false -> "make";
                    Path  -> Path
                end;
            Cmd ->
                case (lists:member($/, Cmd) orelse lists:member($\\, Cmd)) of
                    true  -> Cmd;
                    false -> os:find_executable(Cmd)
                end
        end,
    ERLC_OPTS_Target =
        case Profile of
            normal -> "show-ERLC_OPTS";
            test   -> "show-TEST_ERLC_OPTS"
        end,
    Args = [
        "--no-print-directory",
        "-C", BaseDir,
        "show-ERL_LIBS",
        ERLC_OPTS_Target
    ],
    try
        Port = erlang:open_port({spawn_executable, Make}, [
            {args, Args},
            exit_status, use_stdio, stderr_to_stdout]),
        case erlangmk_port_receive_loop(Port, "", BaseDir) of
            {error, _} ->
                fallback_opts();
            {ok, {ErlLibs, ErlcOpts}} ->
                [code:add_pathsa(filelib:wildcard(
                                   filename:join([ErlLib, "*", "ebin"])))
                 || ErlLib <- ErlLibs],
                ErlcOpts
        end
    catch
        error:_ ->
            fallback_opts()
    end.

erlangmk_port_receive_loop(Port, Stdout, BaseDir) ->
    receive
        {Port, {exit_status, 0}} ->
            erlangmk_format_opts(Stdout, BaseDir);
        {Port, {exit_status, _}} ->
            {error, {erlangmk, make_target_failure}};
        {Port, {data, Out}} ->
            erlangmk_port_receive_loop(Port, Stdout ++ Out, BaseDir)
    end.

erlangmk_format_opts(Stdout, BaseDir) ->
    case string:tokens(Stdout, "\n") of
        [ErlLibsLine | ErlcOptsLines] ->
            ErlLibs = erlangmk_format_erl_libs(ErlLibsLine),
            ErlcOpts = erlangmk_format_erlc_opts(ErlcOptsLines, BaseDir),
            {ok, {ErlLibs, ErlcOpts}};
        _ ->
            {error, {erlangmk, incorrect_output}}
    end.

erlangmk_format_erl_libs(ErlLibsLine) ->
    case os:type() of
        {win32, _} -> string:tokens(ErlLibsLine, ";");
        _          -> string:tokens(ErlLibsLine, ":")
    end.

erlangmk_format_erlc_opts(ErlcOptsLines, BaseDir) ->
    erlangmk_format_erlc_opts(ErlcOptsLines, [], BaseDir).

erlangmk_format_erlc_opts(["+" ++ Option | Rest], Opts, BaseDir) ->
    case make_term(Option) of
        {error, _} -> erlangmk_format_erlc_opts(Rest, Opts, BaseDir);
        Opt        -> erlangmk_format_erlc_opts(Rest, [Opt | Opts], BaseDir)
    end;
erlangmk_format_erlc_opts(["-I" ++ Opt | Rest], Opts, BaseDir)
  when Opt =/= "" ->
    erlangmk_format_erlc_opts(["-I", Opt | Rest], Opts, BaseDir);
erlangmk_format_erlc_opts(["-I", [C | _] = Dir | Rest], Opts, BaseDir)
  when C =/= $- andalso C =/= $+ ->
    AbsDir = filename:absname(Dir, BaseDir),
    erlangmk_format_erlc_opts(Rest, [{i, AbsDir} | Opts], BaseDir);
erlangmk_format_erlc_opts(["-W" ++ Warn | Rest], Opts, BaseDir)
  when Warn =/= "" ->
    erlangmk_format_erlc_opts(["-W", Warn | Rest], Opts, BaseDir);
erlangmk_format_erlc_opts(["-W", Warn | Rest], Opts, BaseDir) ->
    case Warn of
        "all" ->
            erlangmk_format_erlc_opts(Rest, [{warn_format, 999} | Opts],
                BaseDir);
        "error" ->
            erlangmk_format_erlc_opts(Rest, [warnings_as_errors | Opts],
                BaseDir);
        "" ->
            erlangmk_format_erlc_opts(Rest, [{warn_format, 1} | Opts],
                BaseDir);
        _ ->
            try list_to_integer(Warn) of
                Level ->
                    erlangmk_format_erlc_opts(Rest,
                        [{warn_format, Level} | Opts], BaseDir)
            catch
                error:badarg ->
                    erlangmk_format_erlc_opts(Rest, Opts, BaseDir)
            end
    end;
erlangmk_format_erlc_opts(["-D" ++ Opt | Rest], Opts, BaseDir)
  when Opt =/= "" ->
    erlangmk_format_erlc_opts(["-D", Opt | Rest], Opts, BaseDir);
erlangmk_format_erlc_opts(["-D", [C | _] = Val0 | Rest], Opts, BaseDir)
  when C =/= $- andalso C =/= $+ ->
    {Key0, Val1} = split_at_equals(Val0, []),
    Key = list_to_atom(Key0),
    case Val1 of
        [] ->
            erlangmk_format_erlc_opts(Rest, [{d, Key} | Opts], BaseDir);
        Val2 ->
            case make_term(Val2) of
                {error, _} ->
                    erlangmk_format_erlc_opts(Rest, Opts, BaseDir);
                Val ->
                    erlangmk_format_erlc_opts(Rest, [{d, Key, Val} | Opts], BaseDir)
            end
    end;
erlangmk_format_erlc_opts([PathFlag, [_ | _] = Dir | Rest], Opts, BaseDir)
  when PathFlag =:= "-pa" orelse PathFlag =:= "-pz" ->
    AbsDir = filename:absname(Dir, BaseDir),
    case PathFlag of
        "-pa" -> code:add_patha(AbsDir);
        "-pz" -> code:add_pathz(AbsDir)
    end,
    erlangmk_format_erlc_opts(Rest, Opts, BaseDir);
erlangmk_format_erlc_opts([_ | Rest], Opts, BaseDir) ->
    erlangmk_format_erlc_opts(Rest, Opts, BaseDir);
erlangmk_format_erlc_opts([], Opts, _) ->
    lists:reverse(Opts).

%% Function imported from erl_compile.erl from Erlang 19.1.
make_term(Str) ->
    case erl_scan:string(Str) of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens ++ [{dot, 1}]) of
                {ok, Term}      -> Term;
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason, _} ->
            {error, Reason}
    end.

%% Function imported from erl_compile.erl from Erlang 19.1.
split_at_equals([$=|T], Acc) ->
    {lists:reverse(Acc),T};
split_at_equals([H|T], Acc) ->
    split_at_equals(T, [H|Acc]);
split_at_equals([], Acc) ->
    {lists:reverse(Acc),[]}.

fallback_opts() ->
    code:add_pathsa(filelib:wildcard("deps/*/ebin")),
    code:add_pathsa(nested_app_ebins()),
    [
     { i, filename:absname("apps") }, { i, filename:absname("deps") } | [ { i, filename:absname(Path) } || Path <- filelib:wildcard("deps/*/apps")]
    ].

nested_app_ebins() ->
    DetectedAppSrcFiles = filelib:wildcard("deps/*/apps/**/*.app.src"),
    [apps_dir_from_src(AppSrcFile)||AppSrcFile<-DetectedAppSrcFiles].

apps_dir_from_src(SrcFile) ->
    SrcDir = filename:dirname(SrcFile),
    filename:join(SrcDir, "../../ebin").

%% Find the root directory of the project
get_root(Dir) ->
    Path = filename:split(filename:absname(Dir)),
    filename:join(get_root(lists:reverse(Path), Path)).

get_root([], Path) ->
    Path;
%% Strip off /apps/<appname>/src from the end of the path
%% (rebar 3 multi-app project)
get_root(["src", _Appname, "apps" | Tail], _Path) ->
    lists:reverse(Tail);
%% Strip off /src or /test from the end of the path
%% (single-app project)
get_root(["src" | Tail], _Path) ->
    lists:reverse(Tail);
get_root(["test" | Tail], _Path) ->
    lists:reverse(Tail);
get_root([_ | Tail], Path) ->
    get_root(Tail, Path).

translate_paths(Dir, RebarOpts) ->
    [ translate_path(Dir, Opt) || Opt <- RebarOpts ].

translate_path(Dir, {i, Path}) ->
    case Path of
        %% absolute
        "/" ++ _ -> {i, Path};
        %% relative -> make absolute taking rebar.config location into account
        _ -> {i, filename:join([Dir, Path])}
    end;
translate_path(_, Other) -> Other.
