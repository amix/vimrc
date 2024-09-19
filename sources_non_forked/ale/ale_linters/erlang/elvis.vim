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
        \   'sub_type': 'style',
        \})
    endfor

    return l:loclist
endfunction

function! s:AbbreviateMessage(text) abort
    let l:pattern = '\v\c^(line \d+ is too long):.*$'

    return substitute(a:text, l:pattern, '\1.', '')
endfunction

function! s:GetCommand(buffer) abort
    let l:cwd = s:GetCwd(a:buffer)

    let l:file = !empty(l:cwd)
    \   ? expand('#' . a:buffer . ':p')[len(l:cwd) + 1:]
    \   : expand('#' . a:buffer . ':.')

    return '%e rock --output-format=parsable ' . ale#Escape(l:file)
endfunction

function! s:GetCwd(buffer) abort
    let l:markers = ['elvis.config', 'rebar.lock', 'erlang.mk']

    for l:path in ale#path#Upwards(expand('#' . a:buffer . ':p:h'))
        for l:marker in l:markers
            if filereadable(l:path . '/' . l:marker)
                return l:path
            endif
        endfor
    endfor

    return ''
endfunction

call ale#linter#Define('erlang', {
\   'name': 'elvis',
\   'callback': 'ale_linters#erlang#elvis#Handle',
\   'executable': {b -> ale#Var(b, 'erlang_elvis_executable')},
\   'command': function('s:GetCommand'),
\   'cwd': function('s:GetCwd'),
\   'lint_file': 1,
\})
