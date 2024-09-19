" Author: diartyz <diartyz@gmail.com>

call ale#Set('css_stylelint_executable', 'stylelint')
call ale#Set('css_stylelint_options', '')
call ale#Set('css_stylelint_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale_linters#css#stylelint#GetCommand(buffer) abort
    return '%e ' . ale#Pad(ale#Var(a:buffer, 'css_stylelint_options'))
    \   . ' --stdin-filename %s'
endfunction

call ale#linter#Define('css', {
\   'name': 'stylelint',
\   'output_stream': 'both',
\   'executable': {b -> ale#path#FindExecutable(b, 'css_stylelint', [
\       'node_modules/.bin/stylelint',
\   ])},
\   'command': function('ale_linters#css#stylelint#GetCommand'),
\   'callback': 'ale#handlers#css#HandleStyleLintFormat',
\})
