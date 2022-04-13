" Author: diartyz <diartyz@gmail.com>, w0rp <devw0rp@gmail.com>

call ale#Set('less_stylelint_executable', 'stylelint')
call ale#Set('less_stylelint_options', '')
call ale#Set('less_stylelint_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale_linters#less#stylelint#GetCommand(buffer) abort
    let l:options = ale#Var(a:buffer, 'less_stylelint_options')

    return '%e' . ale#Pad(l:options) . ' --stdin-filename %s'
endfunction

call ale#linter#Define('less', {
\   'name': 'stylelint',
\   'executable': {b -> ale#path#FindExecutable(b, 'less_stylelint', [
\       'node_modules/.bin/stylelint',
\   ])},
\   'command': function('ale_linters#less#stylelint#GetCommand'),
\   'callback': 'ale#handlers#css#HandleStyleLintFormat',
\})
