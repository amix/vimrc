" Description: Hurl linter using hurlfmt --check.
" https://hurl.dev/

call ale#Set('hurl_hurlfmt_executable', 'hurlfmt')

function! ale_linters#hurl#hurlfmt#GetCommand(buffer) abort
    return '%e'
    \   . ' --check --no-color '
endfunction

function! ale_linters#hurl#hurlfmt#HandleOutput(buffer, lines) abort
    " Matches patterns:
    "
    " error: Parsing space
    " --> test.hurl:11:48
    " |
    " 8   | header "Content-Type"= "application/json; charset=utf-8"
    " |                      ^ expecting a space
    " |
    "
    " error: Parsing URL
    " --> test.hurl:11:48
    " |
    " 11  | PUT https://jsonplaceholder.typicode.com/posts/{post_id}}
    " |                                                ^ illegal character <{>
    " |
    "
    " Note: hurlfmt seems to report always the first error only so we assume
    " there is only one error to make parsing easier.
    let l:output = []

    if empty(a:lines)
        return l:output
    endif

    let l:pattern = '\v(error|warning): (.+) --\> (.+):(\d+):(\d+) .+ \^ (.+) |'
    let l:lines = join(a:lines, ' ')

    for l:match in ale#util#GetMatches(l:lines, l:pattern)
        call add(l:output, {
        \ 'bufnr': a:buffer,
        \ 'lnum': match[4] + 0,
        \ 'col': match[5] + 0,
        \ 'end_col': match[5] + 0,
        \ 'text': match[2] . ' : ' . match[6],
        \ 'type': (match[1] is# 'error') ? 'E' : 'W'
        \})
    endfor

    return l:output
endfunction

function! ale_linters#hurl#hurlfmt#GetType(severity) abort
    if a:severity is? 'convention'
    \|| a:severity is? 'warning'
    \|| a:severity is? 'refactor'
        return 'W'
    endif

    return 'E'
endfunction

call ale#linter#Define('hurl', {
\   'name': 'hurlfmt',
\   'output_stream': 'stderr',
\   'executable': {b -> ale#Var(b, 'hurl_hurlfmt_executable')},
\   'command': function('ale_linters#hurl#hurlfmt#GetCommand'),
\   'callback': 'ale_linters#hurl#hurlfmt#HandleOutput',
\})
