" Author: Carl Smedstad <carl.smedstad at protonmail dot com>
" Description: az_bicep for bicep files

let g:ale_bicep_az_bicep_executable =
\   get(g:, 'ale_bicep_az_bicep_executable', 'az')

let g:ale_bicep_az_bicep_options =
\   get(g:, 'ale_bicep_az_bicep_options', '')

function! ale_linters#bicep#az_bicep#Executable(buffer) abort
    return ale#Var(a:buffer, 'bicep_az_bicep_executable')
endfunction

function! ale_linters#bicep#az_bicep#Command(buffer) abort
    let l:executable = ale_linters#bicep#az_bicep#Executable(a:buffer)
    let l:options = ale#Var(a:buffer, 'bicep_az_bicep_options')

    if has('win32')
        let l:nullfile = 'NUL'
    else
        let l:nullfile = '/dev/null'
    endif

    return ale#Escape(l:executable)
    \   . ' bicep build --outfile '
    \   . l:nullfile
    \   . ' --file '
    \   . '%s '
    \   . l:options
endfunction

function! ale_linters#bicep#az_bicep#Handle(buffer, lines) abort
    let l:pattern = '\v^([A-Z]+)?(:\s)?(.*)\((\d+),(\d+)\)\s:\s([a-zA-Z]*)\s([-a-zA-Z0-9]*):\s(.*)'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        if l:match[1] is# 'ERROR'
            let l:type = 'E'
        elseif l:match[1] is# 'WARNING'
            let l:type = 'W'
        elseif l:match[6] is# 'Error'
            let l:type = 'E'
        elseif l:match[6] is# 'Warning'
            let l:type = 'W'
        else
            let l:type = 'I'
        endif

        call add(l:output, {
        \   'filename': l:match[3],
        \   'lnum': l:match[4] + 0,
        \   'col': l:match[5] + 0,
        \   'type': l:type,
        \   'code': l:match[7],
        \   'text': l:match[8],
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('bicep', {
\   'name': 'az_bicep',
\   'executable': function('ale_linters#bicep#az_bicep#Executable'),
\   'command': function('ale_linters#bicep#az_bicep#Command'),
\   'callback': 'ale_linters#bicep#az_bicep#Handle',
\   'output_stream': 'stderr',
\   'lint_file': 1,
\})
