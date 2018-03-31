" Author: Alexander Olofsson <alexander.olofsson@liu.se>, Robert Flechtner <flechtner@chemmedia.de>
" Description: puppet-lint for puppet files

let g:ale_puppet_puppetlint_executable =
\   get(g:, 'ale_puppet_puppetlint_executable', 'puppet-lint')

let g:ale_puppet_puppetlint_options =
\   get(g:, 'ale_puppet_puppetlint_options', '--no-autoloader_layout-check')

function! ale_linters#puppet#puppetlint#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'puppet_puppetlint_executable')
endfunction

function! ale_linters#puppet#puppetlint#GetCommand(buffer) abort
    return ale_linters#puppet#puppetlint#GetExecutable(a:buffer)
    \   . ' ' . ale#Var(a:buffer, 'puppet_puppetlint_options')
    \   . ' --log-format "-:%{line}:%{column}: %{kind}: [%{check}] %{message}"'
    \   . ' %t'
endfunction

call ale#linter#Define('puppet', {
\   'name': 'puppetlint',
\   'executable_callback': 'ale_linters#puppet#puppetlint#GetExecutable',
\   'command_callback': 'ale_linters#puppet#puppetlint#GetCommand',
\   'callback': 'ale#handlers#gcc#HandleGCCFormat',
\})
