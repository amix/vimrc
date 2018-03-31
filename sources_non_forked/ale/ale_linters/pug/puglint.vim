" Author: w0rp - <devw0rp@gmail.com>
" Description: pug-lint for checking Pug/Jade files.

call ale#Set('pug_puglint_options', '')
call ale#Set('pug_puglint_executable', 'pug-lint')
call ale#Set('pug_puglint_use_global', 0)

function! ale_linters#pug#puglint#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'pug_puglint', [
    \   'node_modules/.bin/pug-lint',
    \])
endfunction

function! s:FindConfig(buffer) abort
    for l:filename in [
    \   '.pug-lintrc',
    \   '.pug-lintrc.js',
    \   '.pug-lintrc.json',
    \   'package.json',
    \]
        let l:config = ale#path#FindNearestFile(a:buffer, l:filename)

        if !empty(l:config)
            return l:config
        endif
    endfor

    return ''
endfunction

function! ale_linters#pug#puglint#GetCommand(buffer) abort
    let l:executable = ale_linters#pug#puglint#GetExecutable(a:buffer)
    let l:options = ale#Var(a:buffer, 'pug_puglint_options')
    let l:config = s:FindConfig(a:buffer)

    return ale#Escape(l:executable)
    \   . (!empty(l:options) ? ' ' . l:options : '')
    \   . (!empty(l:config) ? ' -c ' . ale#Escape(l:config) : '')
    \   . ' -r inline %t'
endfunction

call ale#linter#Define('pug', {
\   'name': 'puglint',
\   'executable_callback': 'ale_linters#pug#puglint#GetExecutable',
\   'output_stream': 'stderr',
\   'command_callback': 'ale_linters#pug#puglint#GetCommand',
\   'callback': 'ale#handlers#unix#HandleAsError',
\})
