" Author: gagbo <gagbobada@gmail.com>, w0rp <devw0rp@gmail.com>, roel0 <postelmansroel@gmail.com>
" Description: Functions for integrating with C-family linters.

call ale#Set('c_parse_makefile', 0)
let s:sep = has('win32') ? '\' : '/'

function! ale#c#FindProjectRoot(buffer) abort
    for l:project_filename in ['.git/HEAD', 'configure', 'Makefile', 'CMakeLists.txt']
        let l:full_path = ale#path#FindNearestFile(a:buffer, l:project_filename)

        if !empty(l:full_path)
            let l:path = fnamemodify(l:full_path, ':h')

            " Correct .git path detection.
            if fnamemodify(l:path, ':t') is# '.git'
                let l:path = fnamemodify(l:path, ':h')
            endif

            return l:path
        endif
    endfor

    return ''
endfunction

function! ale#c#ParseCFlagsToList(path_prefix, cflags) abort
    let l:cflags_list = []
    let l:previous_options = []

    for l:option in a:cflags
        call add(l:previous_options, l:option)
        " Check if cflag contained a '-' and should not have been splitted
        let l:option_list = split(l:option, '\zs')
        if l:option_list[-1] isnot# ' '
            continue
        endif

        let l:option = join(l:previous_options, '-')
        let l:previous_options = []

        let l:option = '-' . substitute(l:option, '^\s*\(.\{-}\)\s*$', '\1', '')

        " Fix relative paths if needed
        if stridx(l:option, '-I') >= 0 &&
           \ stridx(l:option, '-I' . s:sep) < 0
            let l:rel_path = join(split(l:option, '\zs')[2:], '')
            let l:rel_path = substitute(l:rel_path, '"', '', 'g')
            let l:rel_path = substitute(l:rel_path, '''', '', 'g')
            let l:option = ale#Escape('-I' . a:path_prefix .
                                      \ s:sep . l:rel_path)
        endif

        " Parse the cflag
        if stridx(l:option, '-I') >= 0 ||
           \ stridx(l:option, '-D') >= 0
            if index(l:cflags_list, l:option) < 0
                call add(l:cflags_list, l:option)
            endif
        endif
    endfor

    return l:cflags_list
endfunction

function! ale#c#ParseCFlags(buffer, stdout_make) abort
    if !g:ale_c_parse_makefile
        return []
    endif

    let l:buffer_filename = expand('#' . a:buffer . ':t')
    let l:cflags = []
    for l:lines in split(a:stdout_make, '\\n')
        if stridx(l:lines, l:buffer_filename) >= 0
            let l:cflags = split(l:lines, '-')
            break
        endif
    endfor

    let l:makefile_path = ale#path#FindNearestFile(a:buffer, 'Makefile')
    return ale#c#ParseCFlagsToList(fnamemodify(l:makefile_path, ':p:h'), l:cflags)
endfunction

function! ale#c#GetCFlags(buffer, output) abort
    let l:cflags = ' '

    if g:ale_c_parse_makefile && !empty(a:output)
        let l:cflags = join(ale#c#ParseCFlags(a:buffer, join(a:output, '\n')), ' ') . ' '
    endif

    if l:cflags is# ' '
        let l:cflags = ale#c#IncludeOptions(ale#c#FindLocalHeaderPaths(a:buffer))
    endif

    return l:cflags
endfunction

function! ale#c#GetMakeCommand(buffer) abort
    if g:ale_c_parse_makefile
        let l:makefile_path = ale#path#FindNearestFile(a:buffer, 'Makefile')
        if !empty(l:makefile_path)
            return 'cd '. fnamemodify(l:makefile_path, ':p:h') . ' && make -n'
        endif
    endif

    return ''
endfunction

" Given a buffer number, search for a project root, and output a List
" of directories to include based on some heuristics.
"
" For projects with headers in the project root, the project root will
" be returned.
"
" For projects with an 'include' directory, that directory will be returned.
function! ale#c#FindLocalHeaderPaths(buffer) abort
    let l:project_root = ale#c#FindProjectRoot(a:buffer)

    if empty(l:project_root)
        return []
    endif

    " See if we can find .h files directory in the project root.
    " If we can, that's our include directory.
    if !empty(globpath(l:project_root, '*.h', 0))
        return [l:project_root]
    endif

    " Look for .hpp files too.
    if !empty(globpath(l:project_root, '*.hpp', 0))
        return [l:project_root]
    endif

    " If we find an 'include' directory in the project root, then use that.
    if isdirectory(l:project_root . '/include')
        return [ale#path#Simplify(l:project_root . s:sep . 'include')]
    endif

    return []
endfunction

" Given a List of include paths, create a string containing the -I include
" options for those paths, with the paths escaped for use in the shell.
function! ale#c#IncludeOptions(include_paths) abort
    let l:option_list = []

    for l:path in a:include_paths
        call add(l:option_list, '-I' . ale#Escape(l:path))
    endfor

    if empty(l:option_list)
        return ''
    endif

    return ' ' . join(l:option_list) . ' '
endfunction

let g:ale_c_build_dir_names = get(g:, 'ale_c_build_dir_names', [
\   'build',
\   'bin',
\])

" Given a buffer number, find the build subdirectory with compile commands
" The subdirectory is returned without the trailing /
function! ale#c#FindCompileCommands(buffer) abort
    for l:path in ale#path#Upwards(expand('#' . a:buffer . ':p:h'))
        for l:dirname in ale#Var(a:buffer, 'c_build_dir_names')
            let l:c_build_dir = l:path . s:sep . l:dirname

            if filereadable(l:c_build_dir . '/compile_commands.json')
                return l:c_build_dir
            endif
        endfor
    endfor

    return ''
endfunction
