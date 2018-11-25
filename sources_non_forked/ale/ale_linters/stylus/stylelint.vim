" Author: diartyz <diartyz@gmail.com>, w0rp <devw0rp@gmail.com>

call ale#Set('stylus_stylelint_executable', 'stylelint')
call ale#Set('stylus_stylelint_options', '')
call ale#Set('stylus_stylelint_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale_linters#stylus#stylelint#GetCommand(buffer) abort
    return '%e'
    \   . ale#Pad(ale#Var(a:buffer, 'stylus_stylelint_options'))
    \   . ' --stdin-filename %s'
endfunction

call ale#linter#Define('stylus', {
\   'name': 'stylelint',
\   'executable_callback': ale#node#FindExecutableFunc('stylus_stylelint', [
\       'node_modules/.bin/stylelint',
\   ]),
\   'command_callback': 'ale_linters#stylus#stylelint#GetCommand',
\   'callback': 'ale#handlers#css#HandleStyleLintFormat',
\})
