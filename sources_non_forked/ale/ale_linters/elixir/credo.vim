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

        if l:type is# 'C'
            let l:type = 'E'
        elseif l:type is# 'R'
            let l:type = 'W'
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

call ale#linter#Define('elixir', {
\   'name': 'credo',
\   'executable': 'mix',
\   'command': 'mix help credo && mix credo suggest --format=flycheck --read-from-stdin %s',
\   'callback': 'ale_linters#elixir#credo#Handle',
\})
