" Author: toastal <toastal@protonmail.com>
" Description: `stylelint` linter for SugarSS files

call ale#Set('sugarss_stylelint_executable', 'stylelint')
call ale#Set('sugarss_stylelint_options', '')
call ale#Set('sugarss_stylelint_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale_linters#sugarss#stylelint#GetCommand(buffer) abort
    return '%e ' . ale#Pad(ale#Var(a:buffer, 'sugarss_stylelint_options'))
    \   . ' --syntax=sugarss'
    \   . ' --stdin-filename %s'
endfunction

call ale#linter#Define('sugarss', {
\   'name': 'stylelint',
\   'executable': {b -> ale#path#FindExecutable(b, 'sugarss_stylelint', [
\       'node_modules/.bin/stylelint',
\   ])},
\   'command': function('ale_linters#sugarss#stylelint#GetCommand'),
\   'callback': 'ale#handlers#css#HandleStyleLintFormat',
\})
