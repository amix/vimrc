" Author: diartyz <diartyz@gmail.com>

call ale#Set('scss_stylelint_executable', 'stylelint')
call ale#Set('scss_stylelint_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale_linters#scss#stylelint#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'scss_stylelint', [
    \   'node_modules/.bin/stylelint',
    \])
endfunction

function! ale_linters#scss#stylelint#GetCommand(buffer) abort
    return ale_linters#scss#stylelint#GetExecutable(a:buffer)
    \   . ' --stdin-filename %s'
endfunction

call ale#linter#Define('scss', {
\   'name': 'stylelint',
\   'executable_callback': 'ale_linters#scss#stylelint#GetExecutable',
\   'command_callback': 'ale_linters#scss#stylelint#GetCommand',
\   'callback': 'ale#handlers#css#HandleStyleLintFormat',
\})
