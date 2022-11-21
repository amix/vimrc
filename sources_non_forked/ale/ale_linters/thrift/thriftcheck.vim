" Author: Jon Parise <jon@indelible.org>

call ale#Set('thrift_thriftcheck_executable', 'thriftcheck')
call ale#Set('thrift_thriftcheck_options', '')

function! ale_linters#thrift#thriftcheck#GetCommand(buffer) abort
    return '%e'
    \   . ale#Pad(ale#Var(a:buffer, 'thrift_thriftcheck_options'))
    \   . ' --stdin-filename %s'
    \   . ' %t'
endfunction

function! ale_linters#thrift#thriftcheck#Handle(buffer, lines) abort
    " Matches lines like the following:
    "
    " file.thrift:1:1: error: "py" namespace must match "^idl\\." (namespace.pattern)
    " file.thrift:3:5: warning: 64-bit integer constant -2147483649 may not work in all languages (int.64bit)
    let l:pattern = '\v^[a-zA-Z]?:?[^:]+:(\d+):(\d+): ?([^:]+): (.+) \(([^\)]+)\)$'

    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        if l:match[3] is# 'warning'
            let l:type = 'W'
        else
            let l:type = 'E'
        endif

        call add(l:output, {
        \   'lnum': l:match[1] + 0,
        \   'col': l:match[2] + 0,
        \   'type': l:type,
        \   'text': l:match[4],
        \   'code': l:match[5],
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('thrift', {
\   'name': 'thriftcheck',
\   'executable': {b -> ale#Var(b, 'thrift_thriftcheck_executable')},
\   'command': function('ale_linters#thrift#thriftcheck#GetCommand'),
\   'callback': 'ale_linters#thrift#thriftcheck#Handle',
\})
