" Author: diartyz <diartyz@gmail.com>

call ale#Set('sass_stylelint_executable', 'stylelint')
call ale#Set('sass_stylelint_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale_linters#sass#stylelint#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'sass_stylelint', [
    \   'node_modules/.bin/stylelint',
    \])
endfunction

function! ale_linters#sass#stylelint#GetCommand(buffer) abort
    return ale_linters#sass#stylelint#GetExecutable(a:buffer)
    \   . ' --stdin-filename %s'
endfunction

call ale#linter#Define('sass', {
\   'name': 'stylelint',
\   'executable_callback': 'ale_linters#sass#stylelint#GetExecutable',
\   'command_callback': 'ale_linters#sass#stylelint#GetCommand',
\   'callback': 'ale#handlers#css#HandleStyleLintFormat',
\})
