" Author: w0rp <devw0rp@gmail.com>
" Description: This file adds support for checking CSS code with csslint.

function! ale_linters#css#csslint#GetCommand(buffer) abort
    let l:csslintrc = ale#path#FindNearestFile(a:buffer, '.csslintrc')
    let l:config_option = !empty(l:csslintrc)
    \   ? '--config=' . ale#Escape(l:csslintrc)
    \   : ''

    return 'csslint --format=compact ' . l:config_option . ' %t'
endfunction

call ale#linter#Define('css', {
\   'name': 'csslint',
\   'executable': 'csslint',
\   'command': function('ale_linters#css#csslint#GetCommand'),
\   'callback': 'ale#handlers#css#HandleCSSLintFormat',
\})
