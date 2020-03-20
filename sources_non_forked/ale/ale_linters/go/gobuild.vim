" Author: Joshua Rubin <joshua@rubixconsulting.com>, Ben Reedy <https://github.com/breed808>,
" Jeff Willette <jrwillette88@gmail.com>
" Description: go build for Go files
" inspired by work from dzhou121 <dzhou121@gmail.com>

call ale#Set('go_go_executable', 'go')
call ale#Set('go_gobuild_options', '')

function! ale_linters#go#gobuild#GetCommand(buffer) abort
    let l:options = ale#Var(a:buffer, 'go_gobuild_options')

    " Run go test in local directory with relative path
    return ale#path#BufferCdString(a:buffer)
    \   . ale#go#EnvString(a:buffer)
    \   . ale#Var(a:buffer, 'go_go_executable') . ' test'
    \   . (!empty(l:options) ? ' ' . l:options : '')
    \   . ' -c -o /dev/null ./'
endfunction

function! ale_linters#go#gobuild#GetMatches(lines) abort
    " Matches patterns like the following:
    "
    " file.go:27: missing argument for Printf("%s"): format reads arg 2, have only 1 args
    " file.go:53:10: if block ends with a return statement, so drop this else and outdent its block (move short variable declaration to its own line if necessary)
    " file.go:5:2: expected declaration, found 'STRING' "log"
    " go test returns relative paths so use tail of filename as part of pattern matcher
    let l:pattern = '\v^([a-zA-Z]?:?[^:]+):(\d+):?(\d+)?:? (.+)$'

    return ale#util#GetMatches(a:lines, l:pattern)
endfunction

function! ale_linters#go#gobuild#Handler(buffer, lines) abort
    let l:dir = expand('#' . a:buffer . ':p:h')
    let l:output = []

    for l:match in ale_linters#go#gobuild#GetMatches(a:lines)
        call add(l:output, {
        \   'filename': ale#path#GetAbsPath(l:dir, l:match[1]),
        \   'lnum': l:match[2] + 0,
        \   'col': l:match[3] + 0,
        \   'text': l:match[4],
        \   'type': 'E',
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('go', {
\   'name': 'gobuild',
\   'aliases': ['go build'],
\   'executable': {b -> ale#Var(b, 'go_go_executable')},
\   'command': function('ale_linters#go#gobuild#GetCommand'),
\   'output_stream': 'stderr',
\   'callback': 'ale_linters#go#gobuild#Handler',
\   'lint_file': 1,
\})
