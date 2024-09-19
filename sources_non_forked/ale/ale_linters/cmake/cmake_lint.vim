" Author: Carl Smedstad <carl.smedstad at protonmail dot com>
" Description: cmake-lint for cmake files

let g:ale_cmake_cmake_lint_executable =
\   get(g:, 'ale_cmake_cmake_lint_executable', 'cmake-lint')

let g:ale_cmake_cmake_lint_options =
\   get(g:, 'ale_cmake_cmake_lint_options', '')

function! ale_linters#cmake#cmake_lint#Executable(buffer) abort
    return ale#Var(a:buffer, 'cmake_cmake_lint_executable')
endfunction

function! ale_linters#cmake#cmake_lint#Command(buffer) abort
    let l:executable = ale_linters#cmake#cmake_lint#Executable(a:buffer)
    let l:options = ale#Var(a:buffer, 'cmake_cmake_lint_options')

    return ale#Escape(l:executable) . ' ' . l:options . ' %s'
endfunction

function! ale_linters#cmake#cmake_lint#Handle(buffer, lines) abort
    let l:pattern = '\v^[^:]+:(\d+),?(\d+)?:\s\[([A-Z]\d+)\]\s(.+)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': l:match[1] + 0,
        \   'col': l:match[2] + 0,
        \   'type': 'W',
        \   'code': l:match[3],
        \   'text': l:match[4],
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('cmake', {
\   'name': 'cmake_lint',
\   'executable': function('ale_linters#cmake#cmake_lint#Executable'),
\   'command': function('ale_linters#cmake#cmake_lint#Command'),
\   'callback': 'ale_linters#cmake#cmake_lint#Handle',
\})
