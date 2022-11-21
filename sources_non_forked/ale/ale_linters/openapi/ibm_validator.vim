" Author: Horacio Sanson <hsanson@gmail.com>

call ale#Set('openapi_ibm_validator_executable', 'lint-openapi')
call ale#Set('openapi_ibm_validator_options', '')

function! ale_linters#openapi#ibm_validator#GetCommand(buffer) abort
    return '%e' . ale#Pad(ale#Var(a:buffer, 'openapi_ibm_validator_options'))
    \ . ' %t'
endfunction

function! ale_linters#openapi#ibm_validator#Handle(buffer, lines) abort
    let l:output = []
    let l:type = 'E'
    let l:message = ''
    let l:nr = -1

    for l:line in a:lines
        let l:match = matchlist(l:line, '^errors$')

        if !empty(l:match)
            let l:type = 'E'
        endif

        let l:match = matchlist(l:line, '^warnings$')

        if !empty(l:match)
            let l:type = 'W'
        endif

        let l:match = matchlist(l:line, '^ *Message : *\(.\+\)$')

        if !empty(l:match)
            let l:message = l:match[1]
        endif

        let l:match = matchlist(l:line, '^ *Line *: *\(\d\+\)$')

        if !empty(l:match)
            let l:nr = l:match[1]

            call add(l:output, {
            \   'lnum': l:nr + 0,
            \   'col': 0,
            \   'text': l:message,
            \   'type': l:type,
            \})
        endif
    endfor

    return l:output
endfunction

call ale#linter#Define('openapi', {
\   'name': 'ibm_validator',
\   'executable': {b -> ale#Var(b, 'openapi_ibm_validator_executable')},
\   'command': function('ale_linters#openapi#ibm_validator#GetCommand'),
\   'callback': 'ale_linters#openapi#ibm_validator#Handle',
\})
