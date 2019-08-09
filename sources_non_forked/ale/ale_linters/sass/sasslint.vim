" Author: sQVe - https://github.com/sQVe

call ale#Set('sass_sasslint_executable', 'sass-lint')
call ale#Set('sass_sasslint_options', '')
call ale#Set('sass_sasslint_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale_linters#sass#sasslint#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'sass_sasslint', [
    \   'node_modules/sass-lint/bin/sass-lint.js',
    \   'node_modules/.bin/sass-lint',
    \])
endfunction

function! ale_linters#sass#sasslint#GetCommand(buffer) abort
    let l:executable = ale_linters#sass#sasslint#GetExecutable(a:buffer)
    let l:options = ale#Var(a:buffer, 'sass_sasslint_options')

    return ale#node#Executable(a:buffer, l:executable)
    \   . (!empty(l:options) ? ' ' . l:options : '')
    \   . ' -v -q -f compact %t'
endfunction

call ale#linter#Define('sass', {
\   'name': 'sasslint',
\   'executable': function('ale_linters#sass#sasslint#GetExecutable'),
\   'command': function('ale_linters#sass#sasslint#GetCommand'),
\   'callback': 'ale#handlers#css#HandleCSSLintFormat',
\})
