" Author: Zefei Xuan <https://github.com/zefei>
" Description: Hack type checking (http://hacklang.org/)

function! ale_linters#php#hack#Handle(buffer, lines) abort
    let l:pattern = '^\(.*\):\(\d\+\):\(\d\+\),\(\d\+\): \(.\+])\)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        if a:buffer != bufnr(l:match[1])
            continue
        endif

        call add(l:output, {
        \   'lnum': l:match[2] + 0,
        \   'col': l:match[3] + 0,
        \   'text': l:match[5],
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('php', {
\   'name': 'hack',
\   'executable': 'hh_client',
\   'command': 'hh_client --retries 0 --retry-if-init false',
\   'callback': 'ale_linters#php#hack#Handle',
\})
