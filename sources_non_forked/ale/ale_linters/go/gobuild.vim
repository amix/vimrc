" Author: Joshua Rubin <joshua@rubixconsulting.com>, Ben Reedy <https://github.com/breed808>,
" Jeff Willette <jrwillette88@gmail.com>
" Description: go build for Go files
" inspired by work from dzhou121 <dzhou121@gmail.com>

call ale#Set('go_gobuild_options', '')

function! ale_linters#go#gobuild#ResetEnv() abort
    unlet! s:go_env
endfunction

function! ale_linters#go#gobuild#GoEnv(buffer) abort
    if exists('s:go_env')
        return ''
    endif

    return 'go env GOPATH GOROOT'
endfunction

function! ale_linters#go#gobuild#GetCommand(buffer, goenv_output) abort
    let l:options = ale#Var(a:buffer, 'go_gobuild_options')

    if !exists('s:go_env')
        let s:go_env = {
        \   'GOPATH': a:goenv_output[0],
        \   'GOROOT': a:goenv_output[1],
        \}
    endif

    let l:gopath_env_command = has('win32')
    \   ? 'set GOPATH=' . ale#Escape(s:go_env.GOPATH) . ' && '
    \   : 'GOPATH=' . ale#Escape(s:go_env.GOPATH) . ' '

    " Run go test in local directory with relative path
    return l:gopath_env_command
    \   . ale#path#BufferCdString(a:buffer)
    \   . 'go test'
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
\   'name': 'go build',
\   'executable': 'go',
\   'command_chain': [
\     {'callback': 'ale_linters#go#gobuild#GoEnv', 'output_stream': 'stdout'},
\     {'callback': 'ale_linters#go#gobuild#GetCommand', 'output_stream': 'stderr'},
\   ],
\   'callback': 'ale_linters#go#gobuild#Handler',
\   'lint_file': 1,
\})
