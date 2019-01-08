" Author: gagbo <gagbobada@gmail.com>, w0rp <devw0rp@gmail.com>, roel0 <postelmansroel@gmail.com>
" Description: Functions for integrating with C-family linters.

call ale#Set('c_parse_makefile', 0)
call ale#Set('c_parse_compile_commands', 0)
let s:sep = has('win32') ? '\' : '/'

" Set just so tests can override it.
let g:__ale_c_project_filenames = ['.git/HEAD', 'configure', 'Makefile', 'CMakeLists.txt']

function! ale#c#GetBuildDirectory(buffer) abort
    " Don't include build directory for header files, as compile_commands.json
    " files don't consider headers to be translation units, and provide no
    " commands for compiling header files.
    if expand('#' . a:buffer) =~# '\v\.(h|hpp)$'
        return ''
    endif

    let l:build_dir = ale#Var(a:buffer, 'c_build_dir')

    " c_build_dir has the priority if defined
    if !empty(l:build_dir)
        return l:build_dir
    endif

    return ale#path#Dirname(ale#c#FindCompileCommands(a:buffer))
endfunction


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
    let l:previous_options = ''

    let l:split_lines = split(a:cflag_line, ' ')
    let l:option_index = 0

    while l:option_index < len(l:split_lines)
        let l:option = l:previous_options . l:split_lines[l:option_index]
        let l:option_index = l:option_index + 1

        " Check if cflag contained an unmatched characters and should not have been splitted
        let l:option_special = substitute(l:option, '\\"', '', 'g')
        let l:option_special = substitute(l:option_special, '[^"''()`]', '', 'g')
        let l:option_special = substitute(l:option_special, '""', '', 'g')
        let l:option_special = substitute(l:option_special, '''''', '', 'g')
        let l:option_special = substitute(l:option_special, '``', '', 'g')
        let l:option_special = substitute(l:option_special, '((', '(', 'g')
        let l:option_special = substitute(l:option_special, '))', ')', 'g')
        let l:option_special = substitute(l:option_special, '()', '', 'g')

        if len(l:option_special) > 0 && l:option_index < len(l:split_lines)
            let l:previous_options = l:option . ' '
            continue
        endif

        " Check if there was spaces after -D/-I and the flag should not have been splitted
        if l:option is# '-D' || l:option is# '-I'
            let l:previous_options = l:option
            continue
        endif

        let l:previous_options = ''


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
    endwhile

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

function! s:GetLookupFromCompileCommandsFile(compile_commands_file) abort
    let l:empty = [{}, {}]

    if empty(a:compile_commands_file)
        return l:empty
    endif

    let l:time = getftime(a:compile_commands_file)

    if l:time < 0
        return l:empty
    endif

    let l:key = a:compile_commands_file . ':' . l:time

    if has_key(s:compile_commands_cache, l:key)
        return s:compile_commands_cache[l:key]
    endif

    let l:raw_data = []
    silent! let l:raw_data = json_decode(join(readfile(a:compile_commands_file), ''))

    let l:file_lookup = {}
    let l:dir_lookup = {}

    for l:entry in l:raw_data
        let l:basename = tolower(fnamemodify(l:entry.file, ':t'))
        let l:file_lookup[l:basename] = get(l:file_lookup, l:basename, []) + [l:entry]

        let l:dirbasename = tolower(fnamemodify(l:entry.directory, ':p:h:t'))
        let l:dir_lookup[l:dirbasename] = get(l:dir_lookup, l:basename, []) + [l:entry]
    endfor

    if !empty(l:file_lookup) && !empty(l:dir_lookup)
        let l:result = [l:file_lookup, l:dir_lookup]
        let s:compile_commands_cache[l:key] = l:result

        return l:result
    endif

    return l:empty
endfunction

function! ale#c#ParseCompileCommandsFlags(buffer, dir, file_lookup, dir_lookup) abort
    " Search for an exact file match first.
    let l:basename = tolower(expand('#' . a:buffer . ':t'))
    let l:file_list = get(a:file_lookup, l:basename, [])

    for l:item in l:file_list
        if bufnr(l:item.file) is a:buffer
            return ale#c#ParseCFlags(a:dir, l:item.command)
        endif
    endfor

    " Look for any file in the same directory if we can't find an exact match.
    let l:dir = ale#path#Simplify(expand('#' . a:buffer . ':p:h'))

    let l:dirbasename = tolower(expand('#' . a:buffer . ':p:h:t'))
    let l:dir_list = get(a:dir_lookup, l:dirbasename, [])

    for l:item in l:dir_list
        if ale#path#Simplify(fnamemodify(l:item.file, ':h')) is? l:dir
            return ale#c#ParseCFlags(a:dir, l:item.command)
        endif
    endfor

    return ''
endfunction

function! ale#c#FlagsFromCompileCommands(buffer, compile_commands_file) abort
    let l:dir = ale#path#Dirname(a:compile_commands_file)
    let l:lookups = s:GetLookupFromCompileCommandsFile(a:compile_commands_file)
    let l:file_lookup = l:lookups[0]
    let l:dir_lookup = l:lookups[1]

    return ale#c#ParseCompileCommandsFlags(a:buffer, l:dir, l:file_lookup, l:dir_lookup)
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
