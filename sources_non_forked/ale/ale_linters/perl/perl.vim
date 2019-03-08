" Author: Vincent Lequertier <https://github.com/SkySymbol>
" Description: This file adds support for checking perl syntax

call ale#Set('perl_perl_executable', 'perl')
call ale#Set('perl_perl_options', '-c -Mwarnings -Ilib')

function! ale_linters#perl#perl#GetCommand(buffer) abort
    return '%e' . ale#Pad(ale#Var(a:buffer, 'perl_perl_options')) . ' %t'
endfunction

let s:begin_failed_skip_pattern = '\v' . join([
\   '^Compilation failed in require',
\   '^Can''t locate',
\], '|')

function! ale_linters#perl#perl#Handle(buffer, lines) abort
    if empty(a:lines)
        return []
    endif

    let l:pattern = '\(..\{-}\) at \(..\{-}\) line \(\d\+\)'
    let l:output = []
    let l:basename = expand('#' . a:buffer . ':t')

    let l:type = 'E'

    if a:lines[-1] =~# 'syntax OK'
        let l:type = 'W'
    endif

    let l:seen = {}

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:line = l:match[3]
        let l:file = l:match[2]
        let l:text = l:match[1]

        if ale#path#IsBufferPath(a:buffer, l:file)
        \ && !has_key(l:seen,l:line)
        \ && (
        \   l:text isnot# 'BEGIN failed--compilation aborted'
        \   || empty(l:output)
        \   || match(l:output[-1].text, s:begin_failed_skip_pattern) < 0
        \ )
            call add(l:output, {
            \   'lnum': l:line,
            \   'text': l:text,
            \   'type': l:type,
            \})

            let l:seen[l:line] = 1
        endif
    endfor

    return l:output
endfunction

call ale#linter#Define('perl', {
\   'name': 'perl',
\   'executable': {b -> ale#Var(b, 'perl_perl_executable')},
\   'output_stream': 'both',
\   'command': function('ale_linters#perl#perl#GetCommand'),
\   'callback': 'ale_linters#perl#perl#Handle',
\})
