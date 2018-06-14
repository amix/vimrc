" Author: diartyz <diartyz@gmail.com>

call ale#Set('css_stylelint_executable', 'stylelint')
call ale#Set('css_stylelint_options', '')
call ale#Set('css_stylelint_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale_linters#css#stylelint#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'css_stylelint', [
    \   'node_modules/.bin/stylelint',
    \])
endfunction

function! ale_linters#css#stylelint#GetCommand(buffer) abort
    return ale_linters#css#stylelint#GetExecutable(a:buffer)
    \   . ' ' . ale#Var(a:buffer, 'css_stylelint_options')
    \   . ' --stdin-filename %s'
endfunction

call ale#linter#Define('css', {
\   'name': 'stylelint',
\   'executable_callback': 'ale_linters#css#stylelint#GetExecutable',
\   'command_callback': 'ale_linters#css#stylelint#GetCommand',
\   'callback': 'ale#handlers#css#HandleStyleLintFormat',
\})
