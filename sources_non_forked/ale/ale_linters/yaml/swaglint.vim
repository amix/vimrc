" Author: Matthew Turland <https://github.com/elazar>
" Description: This file adds support for linting Swagger / OpenAPI documents using swaglint

call ale#Set('yaml_swaglint_executable', 'swaglint')
call ale#Set('yaml_swaglint_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale_linters#yaml#swaglint#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'yaml_swaglint', [
    \   'node_modules/.bin/swaglint',
    \])
endfunction

function! ale_linters#yaml#swaglint#GetCommand(buffer) abort
    return ale_linters#yaml#swaglint#GetExecutable(a:buffer)
    \    . ' -r compact --stdin'
endfunction

function! ale_linters#yaml#swaglint#Handle(buffer, lines) abort
    let l:pattern = ': \([^\s]\+\) @ \(\d\+\):\(\d\+\) - \(.\+\)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:obj = {
        \   'type': l:match[1] is# 'error' ? 'E' : 'W',
        \   'lnum': l:match[2] + 0,
        \   'col': l:match[3] + 0,
        \   'text': l:match[4],
        \}

        " Parse the code if it's there.
        let l:code_match = matchlist(l:obj.text, '\v^(.+) \(([^ (]+)\)$')

        if !empty(l:code_match)
            let l:obj.text = l:code_match[1]
            let l:obj.code = l:code_match[2]
        endif

        call add(l:output, l:obj)
    endfor

    return l:output
endfunction

call ale#linter#Define('yaml', {
\   'name': 'swaglint',
\   'executable_callback': 'ale_linters#yaml#swaglint#GetExecutable',
\   'command_callback': 'ale_linters#yaml#swaglint#GetCommand',
\   'callback': 'ale_linters#yaml#swaglint#Handle',
\})
