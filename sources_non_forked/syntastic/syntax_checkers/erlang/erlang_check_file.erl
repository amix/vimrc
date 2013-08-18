#!/usr/bin/env escript
-export([main/1]).

main([FileName]) ->
    LibDirs = filelib:wildcard("{lib,deps}/*/ebin"),
    compile(FileName, LibDirs);
main([FileName | LibDirs]) ->
    compile(FileName, LibDirs).

compile(FileName, LibDirs) ->
    Root = get_root(filename:dirname(FileName)),
    ok = code:add_pathsa(LibDirs),
    compile:file(FileName, [warn_obsolete_guard,
                            warn_unused_import,
                            warn_shadow_vars,
                            warn_export_vars,
                            strong_validation,
                            report,
                            {i, filename:join(Root, "include")},
                            {i, filename:join(Root, "deps")},
                            {i, filename:join(Root, "apps")},
                            {i, filename:join(Root, "lib")}
                        ]).

get_root(Dir) ->
    Path = filename:split(filename:absname(Dir)),
    filename:join(get_root(lists:reverse(Path), Path)).

get_root([], Path) ->
    Path;
get_root(["src" | Tail], _Path) ->
    lists:reverse(Tail);
get_root([_ | Tail], Path) ->
    get_root(Tail, Path).
