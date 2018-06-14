" Author: diartyz <diartyz@gmail.com>, w0rp <devw0rp@gmail.com>

call ale#Set('less_stylelint_executable', 'stylelint')
call ale#Set('less_stylelint_options', '')
call ale#Set('less_stylelint_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale_linters#less#stylelint#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'less_stylelint', [
    \   'node_modules/.bin/stylelint',
    \])
endfunction

function! ale_linters#less#stylelint#GetCommand(buffer) abort
    let l:executable = ale_linters#less#stylelint#GetExecutable(a:buffer)
    let l:options = ale#Var(a:buffer, 'less_stylelint_options')

    return ale#Escape(l:executable)
    \   . (!empty(l:options) ? ' ' . l:options : '')
    \   . ' --stdin-filename %s'
endfunction

call ale#linter#Define('less', {
\   'name': 'stylelint',
\   'executable_callback': 'ale_linters#less#stylelint#GetExecutable',
\   'command_callback': 'ale_linters#less#stylelint#GetCommand',
\   'callback': 'ale#handlers#css#HandleStyleLintFormat',
\})
