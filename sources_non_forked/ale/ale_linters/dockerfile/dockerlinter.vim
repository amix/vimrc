" Author: Shad
" Description: dockerlinter linter for dockerfile

call ale#Set('dockerfile_dockerlinter_executable', 'dockerlinter')
call ale#Set('dockerfile_dockerlinter_options', '')

function! ale_linters#dockerfile#dockerlinter#GetType(type) abort
    if a:type is? 'error'
        return 'E'
    elseif a:type is? 'warning'
        return 'W'
    endif

    return 'I'
endfunction

function! ale_linters#dockerfile#dockerlinter#Handle(buffer, lines) abort
    try
        let l:data = json_decode(join(a:lines, ''))
    catch
        return []
    endtry

    if empty(l:data)
        " Should never happen, but it's better to be on the safe side
        return []
    endif

    let l:messages = []

    for l:object in l:data
        let l:line = get(l:object, 'lineNumber', -1)
        let l:message = l:object['message']
        let l:type = l:object['level']
        let l:detail = l:message
        let l:code = l:object['code']

        if l:code =~# '^SC'
            let l:link = 'https://www.shellcheck.net/wiki/' . l:code
        else
            let l:link = 'https://github.com/buddy-works/dockerfile-linter/blob/master/Rules.md#' . l:code
        endif

        let l:detail = l:message . "\n\n" . l:link

        call add(l:messages, {
        \   'lnum': l:line,
        \   'code': l:code,
        \   'text': l:message,
        \   'type': ale_linters#dockerfile#dockerlinter#GetType(l:type),
        \   'detail': l:detail,
        \})
    endfor

    return l:messages
endfunction

function! ale_linters#dockerfile#dockerlinter#GetCommand(buffer) abort
    return '%e' . ale#Pad(ale#Var(a:buffer, 'dockerfile_dockerlinter_options'))
    \   . ' -j -f'
    \   . ' %t'
endfunction

call ale#linter#Define('dockerfile', {
\   'name': 'dockerlinter',
\   'executable': {b -> ale#Var(b, 'dockerfile_dockerlinter_executable')},
\   'command': function('ale_linters#dockerfile#dockerlinter#GetCommand'),
\   'callback': 'ale_linters#dockerfile#dockerlinter#Handle',
\})
