" Author: jparoz <jesse.paroz@gmail.com>
" Description: hlint for Haskell files

call ale#Set('haskell_hlint_executable', 'hlint')
call ale#Set('haskell_hlint_options', get(g:, 'hlint_options', ''))

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

function! ale_linters#haskell#hlint#GetCommand(buffer) abort
    let l:hlintopts = '--color=never --json'

    return ale#handlers#hlint#GetExecutable(a:buffer)
    \      . ' ' . ale#Var(a:buffer, 'haskell_hlint_options')
    \      . ' ' . l:hlintopts
    \      . ' -'
endfunction

call ale#linter#Define('haskell', {
\   'name': 'hlint',
\   'executable': {b -> ale#Var(b, 'haskell_hlint_executable')},
\   'command': function('ale_linters#haskell#hlint#GetCommand') ,
\   'callback': 'ale_linters#haskell#hlint#Handle',
\})
