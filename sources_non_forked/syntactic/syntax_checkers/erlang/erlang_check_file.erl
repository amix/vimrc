#!/usr/bin/env escript
-export([main/1]).

main([FileName]) ->
    LibDirs = (["ebin", "include", "src", "test"] ++
               filelib:wildcard("{apps,deps,lib}/*/{ebin,include}")),
    compile(FileName, LibDirs);

main([FileName, "-rebar", Path, LibDirs]) ->
    {ok, L} = file:consult(Path),
    P = dict:from_list(L),
    Root = filename:dirname(Path),

    Lib1 = case dict:find(lib_dirs, P) of
             {ok, X} -> lists:map(fun(Sub) -> Root ++ "/" ++ Sub end, X);
             _ -> []
           end,

    Lib2 = case dict:find(sub_dirs, P) of
             {ok, Y} -> lists:foldl(
                          fun(Sub,Sofar) ->
                              Sofar ++ [
                                        Root ++ "/" ++ Sub,
                                        Root ++ "/" ++ Sub ++ "/include",
                                        Root ++ "/" ++ Sub ++ "/deps",
                                        Root ++ "/" ++ Sub ++ "/lib"
                                       ] end, [], Y);
             _ -> []
           end,

    LibDirs1 = LibDirs ++ Lib1 ++ Lib2,
    %io:format("~p~n", [LibDirs1]),
    compile(FileName, LibDirs1);

main([FileName, LibDirs]) ->
    compile(FileName, LibDirs).

compile(FileName, LibDirs) ->
    Root = get_root(filename:dirname(FileName)),
    ok = code:add_pathsa(LibDirs),
    compile:file(FileName,
                 [warn_obsolete_guard,
                  warn_unused_import,
                  warn_shadow_vars,
                  warn_export_vars,
                  strong_validation,
                  report] ++
                 [{i, filename:join(Root, I)} || I <- LibDirs]).

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
