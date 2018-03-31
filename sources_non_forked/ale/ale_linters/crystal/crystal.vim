" Author: Jordan Andree <https://github.com/jordanandree>, David Alexander <opensource@thelonelyghost.com>
" Description: This file adds support for checking Crystal with crystal build

function! ale_linters#crystal#crystal#Handle(buffer, lines) abort
    let l:output = []

    for l:error in ale#util#FuzzyJSONDecode(a:lines, [])
        call add(l:output, {
        \   'lnum': l:error.line + 0,
        \   'col': l:error.column + 0,
        \   'text': l:error.message,
        \})
    endfor

    return l:output
endfunction

function! ale_linters#crystal#crystal#GetCommand(buffer) abort
    return 'crystal build -f json --no-codegen --no-color -o '
    \   . ale#Escape(g:ale#util#nul_file)
    \   . ' %s'
endfunction

call ale#linter#Define('crystal', {
\   'name': 'crystal',
\   'executable': 'crystal',
\   'output_stream': 'both',
\   'lint_file': 1,
\   'command_callback': 'ale_linters#crystal#crystal#GetCommand',
\   'callback': 'ale_linters#crystal#crystal#Handle',
\})
