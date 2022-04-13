" Author: sQVe - https://github.com/sQVe

call ale#Set('scss_sasslint_executable', 'sass-lint')
call ale#Set('scss_sasslint_options', '')
call ale#Set('scss_sasslint_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale_linters#scss#sasslint#GetExecutable(buffer) abort
    return ale#path#FindExecutable(a:buffer, 'scss_sasslint', [
    \   'node_modules/sass-lint/bin/sass-lint.js',
    \   'node_modules/.bin/sass-lint',
    \])
endfunction

function! ale_linters#scss#sasslint#GetCommand(buffer) abort
    let l:executable = ale_linters#scss#sasslint#GetExecutable(a:buffer)
    let l:options = ale#Var(a:buffer, 'scss_sasslint_options')

    return ale#node#Executable(a:buffer, l:executable)
    \   . (!empty(l:options) ? ' ' . l:options : '')
    \   . ' -v -q -f compact %t'
endfunction

call ale#linter#Define('scss', {
\   'name': 'sasslint',
\   'executable': function('ale_linters#scss#sasslint#GetExecutable'),
\   'command': function('ale_linters#scss#sasslint#GetCommand'),
\   'callback': 'ale#handlers#css#HandleCSSLintFormat',
\})
