" Author: Fran C. - https://github.com/franciscoj
" Description: Add dialyzer support for elixir through dialyxir
" https://github.com/jeremyjh/dialyxir

function! ale_linters#elixir#dialyxir#Handle(buffer, lines) abort
    " Matches patterns line the following:
    "
    " lib/filename.ex:19: Function fname/1 has no local return
    let l:pattern = '\v(.+):(\d+): (.+)$'
    let l:output = []
    let l:type = 'W'

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        if bufname(a:buffer) == l:match[1]
            call add(l:output, {
            \   'bufnr': a:buffer,
            \   'lnum': l:match[2] + 0,
            \   'col': 0,
            \   'type': l:type,
            \   'text': l:match[3],
            \})
        endif
    endfor

    return l:output
endfunction

call ale#linter#Define('elixir', {
\   'name': 'dialyxir',
\   'executable': 'mix',
\   'cwd': function('ale#handlers#elixir#FindMixProjectRoot'),
\   'command': 'mix help dialyzer && mix dialyzer',
\   'callback': 'ale_linters#elixir#dialyxir#Handle',
\})
