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
    RebarFile = rebar_file(Dir),
    %% `rebar.config` might contain relative paths.
    %% They are relative to the file! Not to the project root.
    RebarOpts = rebar_opts(Dir ++ "/" ++ RebarFile),
    code:add_patha(filename:absname("ebin")),
    %% `compile:file/2` requires the `{i, Path}` to be relative
    %% to CWD - no surprise here.
    compile:file(File, Defs ++ translate_paths(Dir, RebarOpts));

main(_) ->
    io:format("Usage: ~s <file>~n", [escript:script_name()]),
    halt(1).

rebar_file(Dir) ->
    DirList = filename:split(Dir),
    case lists:last(DirList) of
        "test" ->
            "rebar.test.config";
        _ ->
            "rebar.config"
    end.

rebar_opts(RebarFile) ->
    Dir = get_root(filename:dirname(RebarFile)),
    case file:consult(RebarFile) of
        {ok, Terms} ->
            RebarLibDirs = proplists:get_value(lib_dirs, Terms, []),
            lists:foreach(
                fun(LibDir) ->
                        code:add_pathsa(filelib:wildcard(LibDir ++ "/*/ebin"))
                end, RebarLibDirs),
            RebarDepsDir = proplists:get_value(deps_dir, Terms, "deps"),
            code:add_pathsa(filelib:wildcard(RebarDepsDir ++ "/*/ebin")),
            IncludeDeps = {i, filename:join(Dir, RebarDepsDir)},
            proplists:get_value(erl_opts, Terms, []) ++ [IncludeDeps];
        {error, _} when RebarFile == "rebar.config" ->
          fallback_opts();
        {error, _} ->
            rebar_opts("rebar.config")
    end.

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

get_root(Dir) ->
    Path = filename:split(filename:absname(Dir)),
    filename:join(get_root(lists:reverse(Path), Path)).

get_root([], Path) ->
    Path;
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
