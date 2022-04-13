" Author: Dmitri Vereshchagin <dmitri.vereshchagin@gmail.com>
" Description: Elvis linter for Erlang files

call ale#Set('erlang_elvis_executable', 'elvis')

function! ale_linters#erlang#elvis#Handle(buffer, lines) abort
    let l:pattern = '\v:(\d+):[^:]+:(.+)'
    let l:loclist = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:loclist, {
        \   'lnum': str2nr(l:match[1]),
        \   'text': s:AbbreviateMessage(l:match[2]),
        \   'type': 'W',
        \})
    endfor

    return l:loclist
endfunction

function! s:AbbreviateMessage(text) abort
    let l:pattern = '\v\c^(line \d+ is too long):.*$'

    return substitute(a:text, l:pattern, '\1.', '')
endfunction

function! s:GetCommand(buffer) abort
    let l:file = ale#Escape(expand('#' . a:buffer . ':.'))

    return '%e rock --output-format=parsable ' . l:file
endfunction

call ale#linter#Define('erlang', {
\   'name': 'elvis',
\   'callback': 'ale_linters#erlang#elvis#Handle',
\   'executable': {b -> ale#Var(b, 'erlang_elvis_executable')},
\   'command': function('s:GetCommand'),
\   'lint_file': 1,
\})
