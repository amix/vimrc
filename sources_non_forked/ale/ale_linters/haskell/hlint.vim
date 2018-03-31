" Author: jparoz <jesse.paroz@gmail.com>
" Description: hlint for Haskell files

function! ale_linters#haskell#hlint#Handle(buffer, lines) abort
    let l:output = []

    for l:error in ale#util#FuzzyJSONDecode(a:lines, [])
        if l:error.severity is# 'Error'
            let l:type = 'E'
        elseif l:error.severity is# 'Suggestion'
            let l:type = 'I'
        else
            let l:type = 'W'
        endif

        call add(l:output, {
        \   'lnum': str2nr(l:error.startLine),
        \   'col': str2nr(l:error.startColumn),
        \   'end_lnum': str2nr(l:error.endLine),
        \   'end_col': str2nr(l:error.endColumn),
        \   'text': l:error.severity . ': ' . l:error.hint . '. Found: ' . l:error.from . ' Why not: ' . l:error.to,
        \   'type': l:type,
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('haskell', {
\   'name': 'hlint',
\   'executable': 'hlint',
\   'command': 'hlint --color=never --json -',
\   'callback': 'ale_linters#haskell#hlint#Handle',
\})
