" Description: Handle errors for cppcheck.

function! ale#handlers#cppcheck#GetCwd(buffer) abort
    let [l:dir, l:json_path] = ale#c#FindCompileCommands(a:buffer)

    return !empty(l:dir) ? l:dir : ''
endfunction

function! ale#handlers#cppcheck#GetBufferPathIncludeOptions(buffer) abort
    let l:buffer_path_include = ''

    " Get path to this buffer so we can include it into cppcheck with -I
    " This could be expanded to get more -I directives from the compile
    " command in compile_commands.json, if it's found.
    let l:buffer_path = fnamemodify(bufname(a:buffer), ':p:h')
    let l:buffer_path_include = ' -I' . ale#Escape(l:buffer_path)

    return l:buffer_path_include
endfunction

function! ale#handlers#cppcheck#GetCompileCommandsOptions(buffer) abort
    " If the current buffer is modified, using compile_commands.json does no
    " good, so include the file's directory instead. It's not quite as good as
    " using --project, but is at least equivalent to running cppcheck on this
    " file manually from the file's directory.
    let l:modified = getbufvar(a:buffer, '&modified')

    if l:modified
        return ''
    endif

    " Search upwards from the file for compile_commands.json.
    "
    " If we find it, we'll `cd` to where the compile_commands.json file is,
    " then use the file to set up import paths, etc.
    let [l:dir, l:json_path] = ale#c#FindCompileCommands(a:buffer)

    return !empty(l:json_path)
    \   ? '--project=' . ale#Escape(l:json_path[len(l:dir) + 1: ])
    \   : ''
endfunction

function! ale#handlers#cppcheck#HandleCppCheckFormat(buffer, lines) abort
    " Look for lines like the following.
    "
    "test.cpp:974:6: error: Array 'n[3]' accessed at index 3, which is out of bounds. [arrayIndexOutOfBounds]\
    "    n[3]=3;
    "     ^
    let l:pattern = '\v^(\f+):(\d+):(\d+): (\w+): (.*) \[(\w+)\]\'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        if ale#path#IsBufferPath(a:buffer, l:match[1])
            call add(l:output, {
            \   'lnum':     str2nr(l:match[2]),
            \   'col':      str2nr(l:match[3]),
            \   'type':     l:match[4] is# 'error' ? 'E' : 'W',
            \   'sub_type': l:match[4] is# 'style' ? 'style' : '',
            \   'text':     l:match[5],
            \   'code':     l:match[6]
            \})
        endif
    endfor

    return l:output
endfunction
