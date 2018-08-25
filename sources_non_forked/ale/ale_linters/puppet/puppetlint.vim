" Author: Alexander Olofsson <alexander.olofsson@liu.se>, Robert Flechtner <flechtner@chemmedia.de>
" Description: puppet-lint for puppet files

call ale#Set('puppet_puppetlint_executable', 'puppet-lint')
call ale#Set('puppet_puppetlint_options', '--no-autoloader_layout-check')

function! ale_linters#puppet#puppetlint#GetCommand(buffer) abort
    return '%e' . ale#Pad(ale#Var(a:buffer, 'puppet_puppetlint_options'))
    \   . ' --log-format "-:%{line}:%{column}: %{kind}: [%{check}] %{message}"'
    \   . ' %t'
endfunction

call ale#linter#Define('puppet', {
\   'name': 'puppetlint',
\   'executable_callback': ale#VarFunc('puppet_puppetlint_executable'),
\   'command_callback': 'ale_linters#puppet#puppetlint#GetCommand',
\   'callback': 'ale#handlers#gcc#HandleGCCFormat',
\})
