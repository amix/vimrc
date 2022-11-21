" Author: gagbo <gagbobada@gmail.com>, w0rp <devw0rp@gmail.com>, roel0 <postelmansroel@gmail.com>
" Description: Functions for integrating with C-family linters.

call ale#Set('c_parse_makefile', 0)
call ale#Set('c_always_make', has('unix') && !has('macunix'))
call ale#Set('c_parse_compile_commands', 1)

let s:sep = has('win32') ? '\' : '/'

" Set just so tests can override it.
let g:__ale_c_project_filenames = ['.git/HEAD', 'configure', 'Makefile', 'CMakeLists.txt']

let g:ale_c_build_dir_names = get(g:, 'ale_c_build_dir_names', [
\   'build',
\   'bin',
\])

function! s:CanParseMakefile(buffer) abort
    " Something somewhere seems to delete this setting in tests, so ensure we
    " always have a default value.
    call ale#Set('c_parse_makefile', 0)

    return ale#Var(a:buffer, 'c_parse_makefile')
endfunction

function! ale#c#GetBuildDirectory(buffer) abort
    let l:build_dir = ale#Var(a:buffer, 'c_build_dir')

    " c_build_dir has the priority if defined
    if !empty(l:build_dir)
        return l:build_dir
    endif

    let [l:root, l:json_file] = ale#c#FindCompileCommands(a:buffer)

    return ale#path#Dirname(l:json_file)
endfunction

function! ale#c#ShellSplit(line) abort
    let l:stack = []
    let l:args = ['']
    let l:prev = ''

    for l:char in split(a:line, '\zs')
        if l:char is# ''''
            if len(l:stack) > 0 && get(l:stack, -1) is# ''''
                call remove(l:stack, -1)
            elseif (len(l:stack) == 0 || get(l:stack, -1) isnot# '"') && l:prev isnot# '\'
                call add(l:stack, l:char)
            endif
        elseif (l:char is# '"' || l:char is# '`') && l:prev isnot# '\'
            if len(l:stack) > 0 && get(l:stack, -1) is# l:char
                call remove(l:stack, -1)
            elseif len(l:stack) == 0 || get(l:stack, -1) isnot# ''''
                call add(l:stack, l:char)
            endif
        elseif (l:char is# '(' || l:char is# '[' || l:char is# '{') && l:prev isnot# '\'
            if len(l:stack) == 0 || get(l:stack, -1) isnot# ''''
                call add(l:stack, l:char)
            endif
        elseif (l:char is# ')' || l:char is# ']' || l:char is# '}') && l:prev isnot# '\'
            if len(l:stack) > 0 && get(l:stack, -1) is# {')': '(', ']': '[', '}': '{'}[l:char]
                call remove(l:stack, -1)
            endif
        elseif l:char is# ' ' && len(l:stack) == 0
            if len(get(l:args, -1)) > 0
                call add(l:args, '')
            endif

            continue
        endif

        let l:args[-1] = get(l:args, -1) . l:char
    endfor

    return l:args
endfunction

" Takes the path prefix and a list of cflags and expands @file arguments to
" the contents of the file.
"
" @file arguments are command line arguments recognised by gcc and clang. For
" instance, if @./path/to/file was given to gcc, it would load .path/to/file
" and use the contents of that file as arguments.
function! ale#c#ExpandAtArgs(path_prefix, raw_split_lines) abort
    let l:out_lines = []

    for l:option in a:raw_split_lines
        if stridx(l:option, '@') == 0
            " This is an argument specifying a location of a file containing other arguments
            let l:path = join(split(l:option, '\zs')[1:], '')

            " Make path absolute
            if !ale#path#IsAbsolute(l:path)
                let l:rel_path = substitute(l:path, '"', '', 'g')
                let l:rel_path = substitute(l:rel_path, '''', '', 'g')
                let l:path = ale#path#GetAbsPath(a:path_prefix, l:rel_path)
            endif

            " Read the file and add all the arguments
            try
                let l:additional_args = readfile(l:path)
            catch
                continue " All we can really do is skip this argument
            endtry

            let l:file_lines = []

            for l:line in l:additional_args
                let l:file_lines += ale#c#ShellSplit(l:line)
            endfor

            " @file arguments can include other @file arguments, so we must
            " recurse.
            let l:out_lines += ale#c#ExpandAtArgs(a:path_prefix, l:file_lines)
        else
            " This is not an @file argument, so don't touch it.
            let l:out_lines += [l:option]
        endif
    endfor

    return l:out_lines
endfunction

" Quote C/C++ a compiler argument, if needed.
"
" Quoting arguments might cause issues with some systems/compilers, so we only
" quote them if we need to.
function! ale#c#QuoteArg(arg) abort
    if a:arg !~# '\v[#$&*()\\|[\]{};''"<>/?! ^%]'
        return a:arg
    endif

    return ale#Escape(a:arg)
endfunction

function! ale#c#ParseCFlags(path_prefix, should_quote, raw_arguments) abort
    " Expand @file arguments now before parsing
    let l:arguments = ale#c#ExpandAtArgs(a:path_prefix, a:raw_arguments)
    " A list of [already_quoted, argument]
    let l:items = []
    let l:option_index = 0

    while l:option_index < len(l:arguments)
        let l:option = l:arguments[l:option_index]
        let l:option_index = l:option_index + 1

        " Include options, that may need relative path fix
        if stridx(l:option, '-I') == 0
        \ || stridx(l:option, '-iquote') == 0
        \ || stridx(l:option, '-isystem') == 0
        \ || stridx(l:option, '-idirafter') == 0
        \ || stridx(l:option, '-iframework') == 0
            if stridx(l:option, '-I') == 0 && l:option isnot# '-I'
                let l:arg = join(split(l:option, '\zs')[2:], '')
                let l:option = '-I'
            else
                let l:arg = l:arguments[l:option_index]
                let l:option_index = l:option_index + 1
            endif

            " Fix relative paths if needed
            if !ale#path#IsAbsolute(l:arg)
                let l:rel_path = substitute(l:arg, '"', '', 'g')
                let l:rel_path = substitute(l:rel_path, '''', '', 'g')
                let l:arg = ale#path#GetAbsPath(a:path_prefix, l:rel_path)
            endif

            call add(l:items, [1, l:option])
            call add(l:items, [1, ale#Escape(l:arg)])
        " Options with arg that can be grouped with the option or separate
        elseif stridx(l:option, '-D') == 0 || stridx(l:option, '-B') == 0
            if l:option is# '-D' || l:option is# '-B'
                call add(l:items, [1, l:option])
                call add(l:items, [0, l:arguments[l:option_index]])
                let l:option_index = l:option_index + 1
            else
                call add(l:items, [0, l:option])
            endif
        " Options that have an argument (always separate)
        elseif l:option is# '-iprefix' || stridx(l:option, '-iwithprefix') == 0
        \ || l:option is# '-isysroot' || l:option is# '-imultilib'
        \ || l:option is# '-include' || l:option is# '-imacros'
            call add(l:items, [0, l:option])
            call add(l:items, [0, l:arguments[l:option_index]])
            let l:option_index = l:option_index + 1
        " Options without argument
        elseif (stridx(l:option, '-W') == 0 && stridx(l:option, '-Wa,') != 0 && stridx(l:option, '-Wl,') != 0 && stridx(l:option, '-Wp,') != 0)
        \ || l:option is# '-w' || stridx(l:option, '-pedantic') == 0
        \ || l:option is# '-ansi' || stridx(l:option, '-std=') == 0
        \ || stridx(l:option, '-f') == 0 && l:option !~# '\v^-f(dump|diagnostics|no-show-column|stack-usage)'
        \ || stridx(l:option, '-O') == 0
        \ || l:option is# '-C' || l:option is# '-CC' || l:option is# '-trigraphs'
        \ || stridx(l:option, '-nostdinc') == 0 || stridx(l:option, '-iplugindir=') == 0
        \ || stridx(l:option, '--sysroot=') == 0 || l:option is# '--no-sysroot-suffix'
        \ || stridx(l:option, '-m') == 0
            call add(l:items, [0, l:option])
        endif
    endwhile

    if a:should_quote
        " Quote C arguments that haven't already been quoted above.
        " If and only if we've been asked to quote them.
        call map(l:items, 'v:val[0] ? v:val[1] : ale#c#QuoteArg(v:val[1])')
    else
        call map(l:items, 'v:val[1]')
    endif

    return join(l:items, ' ')
endfunction

function! ale#c#ParseCFlagsFromMakeOutput(buffer, make_output) abort
    if !s:CanParseMakefile(a:buffer)
        return v:null
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

    return ale#c#ParseCFlags(l:makefile_dir, 0, ale#c#ShellSplit(l:cflag_line))
endfunction

" Given a buffer number, find the project directory containing
" compile_commands.json, and the path to the compile_commands.json file.
"
" If compile_commands.json cannot be found, two empty strings will be
" returned.
function! ale#c#FindCompileCommands(buffer) abort
    " Look above the current source file to find compile_commands.json
    let l:json_file = ale#path#FindNearestFile(a:buffer, 'compile_commands.json')

    if !empty(l:json_file)
        return [fnamemodify(l:json_file, ':h'), l:json_file]
    endif

    " Search in build directories if we can't find it in the project.
    for l:path in ale#path#Upwards(expand('#' . a:buffer . ':p:h'))
        for l:dirname in ale#Var(a:buffer, 'c_build_dir_names')
            let l:c_build_dir = l:path . s:sep . l:dirname
            let l:json_file = l:c_build_dir . s:sep . 'compile_commands.json'

            if filereadable(l:json_file)
                return [l:path, l:json_file]
            endif
        endfor
    endfor

    return ['', '']
endfunction

" Find the project root for C/C++ projects.
"
" The location of compile_commands.json will be used to find project roots.
"
" If compile_commands.json cannot be found, other common configuration files
" will be used to detect the project root.
function! ale#c#FindProjectRoot(buffer) abort
    let [l:root, l:json_file] = ale#c#FindCompileCommands(a:buffer)

    " Fall back on detecting the project root based on other filenames.
    if empty(l:root)
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
    endif

    return l:root
endfunction

" Cache compile_commands.json data in a Dictionary, so we don't need to read
" the same files over and over again. The key in the dictionary will include
" the last modified time of the file.
if !exists('s:compile_commands_cache')
    let s:compile_commands_cache = {}
endif

function! ale#c#ResetCompileCommandsCache() abort
    let s:compile_commands_cache = {}
endfunction

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

    if type(l:raw_data) isnot v:t_list
        let l:raw_data = []
    endif

    let l:file_lookup = {}
    let l:dir_lookup = {}

    for l:entry in (type(l:raw_data) is v:t_list ? l:raw_data : [])
        let l:filename = ale#path#GetAbsPath(l:entry.directory, l:entry.file)

        " Store a key for lookups by the absolute path to the filename.
        let l:file_lookup[l:filename] = get(l:file_lookup, l:filename, []) + [l:entry]

        " Store a key for fuzzy lookups by the absolute path to the directory.
        let l:dirname = fnamemodify(l:filename, ':h')
        let l:dir_lookup[l:dirname] = get(l:dir_lookup, l:dirname, []) + [l:entry]

        " Store a key for fuzzy lookups by just the basename of the file.
        let l:basename = tolower(fnamemodify(l:entry.file, ':t'))
        let l:file_lookup[l:basename] = get(l:file_lookup, l:basename, []) + [l:entry]

        " Store a key for fuzzy lookups by just the basename of the directory.
        let l:dirbasename = tolower(fnamemodify(l:entry.directory, ':p:h:t'))
        let l:dir_lookup[l:dirbasename] = get(l:dir_lookup, l:dirbasename, []) + [l:entry]
    endfor

    if !empty(l:file_lookup) && !empty(l:dir_lookup)
        let l:result = [l:file_lookup, l:dir_lookup]
        let s:compile_commands_cache[l:key] = l:result

        return l:result
    endif

    return l:empty
endfunction

" Get [should_quote, arguments] from either 'command' or 'arguments'
" 'arguments' should be quoted later, the split 'command' strings should not.
function! s:GetArguments(json_item) abort
    if has_key(a:json_item, 'arguments')
        return [1, a:json_item.arguments]
    elseif has_key(a:json_item, 'command')
        return [0, ale#c#ShellSplit(a:json_item.command)]
    endif

    return [0, []]
endfunction

function! ale#c#ParseCompileCommandsFlags(buffer, file_lookup, dir_lookup) abort
    let l:buffer_filename = ale#path#Simplify(expand('#' . a:buffer . ':p'))
    let l:basename = tolower(fnamemodify(l:buffer_filename, ':t'))
    " Look for any file in the same directory if we can't find an exact match.
    let l:dir = fnamemodify(l:buffer_filename, ':h')

    " Search for an exact file match first.
    let l:file_list = get(a:file_lookup, l:buffer_filename, [])

    " We may have to look for /foo/bar instead of C:\foo\bar
    if empty(l:file_list) && has('win32')
        let l:file_list = get(
        \   a:file_lookup,
        \   ale#path#RemoveDriveLetter(l:buffer_filename),
        \   []
        \)
    endif

    " Try the absolute path to the directory second.
    let l:dir_list = get(a:dir_lookup, l:dir, [])

    if empty(l:dir_list) && has('win32')
        let l:dir_list = get(
        \   a:dir_lookup,
        \   ale#path#RemoveDriveLetter(l:dir),
        \   []
        \)
    endif

    if empty(l:file_list) && empty(l:dir_list)
        " If we can't find matches with the path to the file, try a
        " case-insensitive match for any similarly-named file.
        let l:file_list = get(a:file_lookup, l:basename, [])

        " If we can't find matches with the path to the directory, try a
        " case-insensitive match for anything in similarly-named directory.
        let l:dir_list = get(a:dir_lookup, tolower(fnamemodify(l:dir, ':t')), [])
    endif

    " A source file matching the header filename.
    let l:source_file = ''

    if empty(l:file_list) && l:basename =~? '\.h$\|\.hpp$'
        for l:suffix in ['.c', '.cpp']
            " Try to find a source file by an absolute path first.
            let l:key = fnamemodify(l:buffer_filename, ':r') . l:suffix
            let l:file_list = get(a:file_lookup, l:key, [])

            if empty(l:file_list) && has('win32')
                let l:file_list = get(
                \   a:file_lookup,
                \   ale#path#RemoveDriveLetter(l:key),
                \   []
                \)
            endif

            if empty(l:file_list)
                " Look fuzzy matches on the basename second.
                let l:key = fnamemodify(l:basename, ':r') . l:suffix
                let l:file_list = get(a:file_lookup, l:key, [])
            endif

            if !empty(l:file_list)
                let l:source_file = l:key
                break
            endif
        endfor
    endif

    for l:item in l:file_list
        let l:filename = ale#path#GetAbsPath(l:item.directory, l:item.file)

        " Load the flags for this file, or for a source file matching the
        " header file.
        if (
        \   bufnr(l:filename) is a:buffer
        \   || (
        \       !empty(l:source_file)
        \       && l:filename[-len(l:source_file):] is? l:source_file
        \   )
        \)
            let [l:should_quote, l:args] = s:GetArguments(l:item)

            return ale#c#ParseCFlags(l:item.directory, l:should_quote, l:args)
        endif
    endfor

    for l:item in l:dir_list
        let l:filename = ale#path#GetAbsPath(l:item.directory, l:item.file)

        if ale#path#RemoveDriveLetter(fnamemodify(l:filename, ':h'))
        \  is? ale#path#RemoveDriveLetter(l:dir)
            let [l:should_quote, l:args] = s:GetArguments(l:item)

            return ale#c#ParseCFlags(l:item.directory, l:should_quote, l:args)
        endif
    endfor

    return ''
endfunction

function! ale#c#FlagsFromCompileCommands(buffer, compile_commands_file) abort
    let l:lookups = s:GetLookupFromCompileCommandsFile(a:compile_commands_file)
    let l:file_lookup = l:lookups[0]
    let l:dir_lookup = l:lookups[1]

    return ale#c#ParseCompileCommandsFlags(a:buffer, l:file_lookup, l:dir_lookup)
endfunction

function! ale#c#GetCFlags(buffer, output) abort
    let l:cflags = v:null

    if ale#Var(a:buffer, 'c_parse_compile_commands')
        let [l:root, l:json_file] = ale#c#FindCompileCommands(a:buffer)

        if !empty(l:json_file)
            let l:cflags = ale#c#FlagsFromCompileCommands(a:buffer, l:json_file)
        endif
    endif

    if empty(l:cflags) && s:CanParseMakefile(a:buffer) && !empty(a:output)
        let l:cflags = ale#c#ParseCFlagsFromMakeOutput(a:buffer, a:output)
    endif

    if l:cflags is v:null
        let l:cflags = ale#c#IncludeOptions(ale#c#FindLocalHeaderPaths(a:buffer))
    endif

    return l:cflags isnot v:null ? l:cflags : ''
endfunction

function! ale#c#GetMakeCommand(buffer) abort
    if s:CanParseMakefile(a:buffer)
        let l:path = ale#path#FindNearestFile(a:buffer, 'Makefile')

        if empty(l:path)
            let l:path = ale#path#FindNearestFile(a:buffer, 'GNUmakefile')
        endif

        if !empty(l:path)
            let l:always_make = ale#Var(a:buffer, 'c_always_make')

            return [
            \   fnamemodify(l:path, ':h'),
            \   'make -n' . (l:always_make ? ' --always-make' : ''),
            \]
        endif
    endif

    return ['', '']
endfunction

function! ale#c#RunMakeCommand(buffer, Callback) abort
    let [l:cwd, l:command] = ale#c#GetMakeCommand(a:buffer)

    if empty(l:command)
        return a:Callback(a:buffer, [])
    endif

    return ale#command#Run(
    \   a:buffer,
    \   l:command,
    \   {b, output -> a:Callback(a:buffer, output)},
    \   {'cwd': l:cwd},
    \)
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

" Get the language flag depending on on the executable, options and
" file extension
function! ale#c#GetLanguageFlag(
\   buffer,
\   executable,
\   use_header_lang_flag,
\   header_exts,
\   linter_lang_flag
\) abort
    " Use only '-header' if the executable is 'clang' by default
    if a:use_header_lang_flag == -1
        let l:use_header_lang_flag = a:executable =~# 'clang'
    else
        let l:use_header_lang_flag = a:use_header_lang_flag
    endif

    " If we don't use the header language flag, return the default linter
    " language flag
    if !l:use_header_lang_flag
        return a:linter_lang_flag
    endif

    " Get the buffer file extension
    let l:buf_ext = expand('#' . a:buffer . ':e')

    " If the buffer file is an header according to its extension, use
    " the linter language flag + '-header', ex: 'c-header'
    if index(a:header_exts, l:buf_ext) >= 0
        return a:linter_lang_flag . '-header'
    endif

    " Else, use the default linter language flag
    return a:linter_lang_flag
endfunction
