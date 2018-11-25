" Author: gagbo <gagbobada@gmail.com>, w0rp <devw0rp@gmail.com>, roel0 <postelmansroel@gmail.com>
" Description: Functions for integrating with C-family linters.

call ale#Set('c_parse_makefile', 0)
call ale#Set('c_parse_compile_commands', 0)
let s:sep = has('win32') ? '\' : '/'

" Set just so tests can override it.
let g:__ale_c_project_filenames = ['.git/HEAD', 'configure', 'Makefile', 'CMakeLists.txt']

function! ale#c#FindProjectRoot(buffer) abort
    for l:project_filename in g:__ale_c_project_filenames
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

function! ale#c#ParseCFlags(path_prefix, cflag_line) abort
    let l:cflags_list = []
    let l:previous_options = []

    for l:option in split(a:cflag_line, '-')
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

    return join(l:cflags_list, ' ')
endfunction

function! ale#c#ParseCFlagsFromMakeOutput(buffer, make_output) abort
    if !g:ale_c_parse_makefile
        return ''
    endif

    let l:buffer_filename = expand('#' . a:buffer . ':t')
    let l:cflag_line = ''

    " Find a line matching this buffer's filename in the make output.
    for l:line in a:make_output
        if stridx(l:line, l:buffer_filename) >= 0
            let l:cflag_line = l:line
            break
        endif
    endfor

    let l:makefile_path = ale#path#FindNearestFile(a:buffer, 'Makefile')
    let l:makefile_dir = fnamemodify(l:makefile_path, ':p:h')

    return ale#c#ParseCFlags(l:makefile_dir, l:cflag_line)
endfunction

" Given a buffer number, find the build subdirectory with compile commands
" The subdirectory is returned without the trailing /
function! ale#c#FindCompileCommands(buffer) abort
    " Look above the current source file to find compile_commands.json
    let l:json_file = ale#path#FindNearestFile(a:buffer, 'compile_commands.json')

    if !empty(l:json_file)
        return l:json_file
    endif

    " Search in build directories if we can't find it in the project.
    for l:path in ale#path#Upwards(expand('#' . a:buffer . ':p:h'))
        for l:dirname in ale#Var(a:buffer, 'c_build_dir_names')
            let l:c_build_dir = l:path . s:sep . l:dirname
            let l:json_file = l:c_build_dir . s:sep . 'compile_commands.json'

            if filereadable(l:json_file)
                return l:json_file
            endif
        endfor
    endfor

    return ''
endfunction

" Cache compile_commands.json data in a Dictionary, so we don't need to read
" the same files over and over again. The key in the dictionary will include
" the last modified time of the file.
if !exists('s:compile_commands_cache')
    let s:compile_commands_cache = {}
endif

function! s:GetListFromCompileCommandsFile(compile_commands_file) abort
    if empty(a:compile_commands_file)
        return []
    endif

    let l:time = getftime(a:compile_commands_file)

    if l:time < 0
        return []
    endif

    let l:key = a:compile_commands_file . ':' . l:time

    if has_key(s:compile_commands_cache, l:key)
        return s:compile_commands_cache[l:key]
    endif

    let l:data = []
    silent! let l:data = json_decode(join(readfile(a:compile_commands_file), ''))

    if !empty(l:data)
        let s:compile_commands_cache[l:key] = l:data

        return l:data
    endif

    return []
endfunction

function! ale#c#ParseCompileCommandsFlags(buffer, dir, json_list) abort
    " Search for an exact file match first.
    for l:item in a:json_list
        if bufnr(l:item.file) is a:buffer
            return ale#c#ParseCFlags(a:dir, l:item.command)
        endif
    endfor

    " Look for any file in the same directory if we can't find an exact match.
    let l:dir = ale#path#Simplify(expand('#' . a:buffer . ':p:h'))

    for l:item in a:json_list
        if ale#path#Simplify(fnamemodify(l:item.file, ':h')) is? l:dir
            return ale#c#ParseCFlags(a:dir, l:item.command)
        endif
    endfor

    return ''
endfunction

function! ale#c#FlagsFromCompileCommands(buffer, compile_commands_file) abort
    let l:dir = ale#path#Dirname(a:compile_commands_file)
    let l:json_list = s:GetListFromCompileCommandsFile(a:compile_commands_file)

    return ale#c#ParseCompileCommandsFlags(a:buffer, l:dir, l:json_list)
endfunction

function! ale#c#GetCFlags(buffer, output) abort
    let l:cflags = ' '

    if ale#Var(a:buffer, 'c_parse_makefile') && !empty(a:output)
        let l:cflags = ale#c#ParseCFlagsFromMakeOutput(a:buffer, a:output)
    endif

    if ale#Var(a:buffer, 'c_parse_compile_commands')
        let l:json_file = ale#c#FindCompileCommands(a:buffer)

        if !empty(l:json_file)
            let l:cflags = ale#c#FlagsFromCompileCommands(a:buffer, l:json_file)
        endif
    endif

    if l:cflags is# ' '
        let l:cflags = ale#c#IncludeOptions(ale#c#FindLocalHeaderPaths(a:buffer))
    endif

    return l:cflags
endfunction

function! ale#c#GetMakeCommand(buffer) abort
    if ale#Var(a:buffer, 'c_parse_makefile')
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

    return join(l:option_list)
endfunction

let g:ale_c_build_dir_names = get(g:, 'ale_c_build_dir_names', [
\   'build',
\   'bin',
\])
