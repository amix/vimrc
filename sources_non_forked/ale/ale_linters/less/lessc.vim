" Author: zanona <https://github.com/zanona>, w0rp <devw0rp@gmail.com>
" Description: This file adds support for checking Less code with lessc.

call ale#Set('less_lessc_executable', 'lessc')
call ale#Set('less_lessc_options', '')
call ale#Set('less_lessc_use_global', 0)

function! ale_linters#less#lessc#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'less_lessc', [
    \   'node_modules/.bin/lessc',
    \])
endfunction

function! ale_linters#less#lessc#GetCommand(buffer) abort
    let l:executable = ale_linters#less#lessc#GetExecutable(a:buffer)
    let l:dir = expand('#' . a:buffer . ':p:h')
    let l:options = ale#Var(a:buffer, 'less_lessc_options')

    return ale#Escape(l:executable)
    \   . ' --no-color --lint'
    \   . ' --include-path=' . ale#Escape(l:dir)
    \   . (!empty(l:options) ? ' ' . l:options : '')
    \   . ' -'
endfunction

function! ale_linters#less#lessc#Handle(buffer, lines) abort
    let l:dir = expand('#' . a:buffer . ':p:h')
    " Matches patterns like the following:
    let l:pattern = '^\(\w\+\): \(.\{-}\) in \(.\{-}\) on line \(\d\+\), column \(\d\+\):$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:item = {
        \   'lnum': l:match[4] + 0,
        \   'col': l:match[5] + 0,
        \   'text': l:match[2],
        \   'type': 'E',
        \}

        if l:match[3] isnot# '-'
            let l:item.filename = ale#path#GetAbsPath(l:dir, l:match[3])
        endif

        call add(l:output, l:item)
    endfor

    return l:output
endfunction

call ale#linter#Define('less', {
\   'name': 'lessc',
\   'executable_callback': 'ale_linters#less#lessc#GetExecutable',
\   'command_callback': 'ale_linters#less#lessc#GetCommand',
\   'callback': 'ale_linters#less#lessc#Handle',
\   'output_stream': 'stderr',
\})
