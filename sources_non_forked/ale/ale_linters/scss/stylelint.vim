" Author: diartyz <diartyz@gmail.com>

call ale#Set('scss_stylelint_executable', 'stylelint')
call ale#Set('scss_stylelint_options', '')
call ale#Set('scss_stylelint_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale_linters#scss#stylelint#GetCommand(buffer) abort
    return '%e ' . ale#Pad(ale#Var(a:buffer, 'scss_stylelint_options'))
    \   . ' --stdin-filename %s'
endfunction

call ale#linter#Define('scss', {
\   'name': 'stylelint',
\   'executable': {b -> ale#node#FindExecutable(b, 'scss_stylelint', [
\       'node_modules/.bin/stylelint',
\   ])},
\   'command': function('ale_linters#scss#stylelint#GetCommand'),
\   'callback': 'ale#handlers#css#HandleStyleLintFormat',
\})
