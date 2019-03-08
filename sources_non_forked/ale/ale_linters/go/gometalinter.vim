" Author: Ben Reedy <https://github.com/breed808>, Jeff Willette <jrwillette88@gmail.com>
" Description: Adds support for the gometalinter suite for Go files

call ale#Set('go_gometalinter_options', '')
call ale#Set('go_gometalinter_executable', 'gometalinter')
call ale#Set('go_gometalinter_lint_package', 0)

function! ale_linters#go#gometalinter#GetCommand(buffer) abort
    let l:filename = expand('#' . a:buffer . ':t')
    let l:options = ale#Var(a:buffer, 'go_gometalinter_options')
    let l:lint_package = ale#Var(a:buffer, 'go_gometalinter_lint_package')

    " BufferCdString is used so that we can be sure the paths output from gometalinter can
    " be calculated to absolute paths in the Handler
    if l:lint_package
        return ale#path#BufferCdString(a:buffer)
        \   . '%e'
        \   . (!empty(l:options) ? ' ' . l:options : '') . ' .'
    endif

    return ale#path#BufferCdString(a:buffer)
    \   . '%e'
    \   . ' --include=' . ale#Escape(ale#util#EscapePCRE(l:filename))
    \   . (!empty(l:options) ? ' ' . l:options : '') . ' .'
endfunction

function! ale_linters#go#gometalinter#GetMatches(lines) abort
    let l:pattern = '\v^([a-zA-Z]?:?[^:]+):(\d+):?(\d+)?:?:?(warning|error):?\s\*?(.+)$'

    return ale#util#GetMatches(a:lines, l:pattern)
endfunction

function! ale_linters#go#gometalinter#Handler(buffer, lines) abort
    let l:dir = expand('#' . a:buffer . ':p:h')
    let l:output = []

    for l:match in ale_linters#go#gometalinter#GetMatches(a:lines)
        " l:match[1] will already be an absolute path, output from gometalinter
        call add(l:output, {
        \   'filename': ale#path#GetAbsPath(l:dir, l:match[1]),
        \   'lnum': l:match[2] + 0,
        \   'col': l:match[3] + 0,
        \   'type': tolower(l:match[4]) is# 'warning' ? 'W' : 'E',
        \   'text': l:match[5],
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('go', {
\   'name': 'gometalinter',
\   'executable': {b -> ale#Var(b, 'go_gometalinter_executable')},
\   'command': function('ale_linters#go#gometalinter#GetCommand'),
\   'callback': 'ale_linters#go#gometalinter#Handler',
\   'lint_file': 1,
\})
