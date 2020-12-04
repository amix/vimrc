" Author: hauleth - https://github.com/hauleth

function! ale_linters#elixir#credo#Handle(buffer, lines) abort
    " Matches patterns line the following:
    "
    " lib/filename.ex:19:7: F: Pipe chain should start with a raw value.
    let l:pattern = '\v:(\d+):?(\d+)?: (.): (.+)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:type = l:match[3]
        let l:text = l:match[4]

        " Refactoring opportunities
        if l:type is# 'F'
            let l:type = 'W'
        " Consistency
        elseif l:type is# 'C'
            let l:type = 'W'
        " Software Design
        elseif l:type is# 'D'
            let l:type = 'I'
        " Code Readability
        elseif l:type is# 'R'
            let l:type = 'I'
        endif

        call add(l:output, {
        \   'bufnr': a:buffer,
        \   'lnum': l:match[1] + 0,
        \   'col': l:match[2] + 0,
        \   'type': l:type,
        \   'text': l:text,
        \})
    endfor

    return l:output
endfunction

function! ale_linters#elixir#credo#GetMode() abort
    if get(g:, 'ale_elixir_credo_strict', 0)
        return '--strict'
    else
        return 'suggest'
    endif
endfunction

function! ale_linters#elixir#credo#GetCommand(buffer) abort
    let l:project_root = ale#handlers#elixir#FindMixUmbrellaRoot(a:buffer)
    let l:mode = ale_linters#elixir#credo#GetMode()

    return ale#path#CdString(l:project_root)
    \ . 'mix help credo && '
    \ . 'mix credo ' . ale_linters#elixir#credo#GetMode()
    \ . ' --format=flycheck --read-from-stdin %s'
endfunction

call ale#linter#Define('elixir', {
\   'name': 'credo',
\   'executable': 'mix',
\   'command': function('ale_linters#elixir#credo#GetCommand'),
\   'callback': 'ale_linters#elixir#credo#Handle',
\})
