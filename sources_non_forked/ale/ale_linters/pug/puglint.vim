" Author: w0rp - <devw0rp@gmail.com>
" Description: pug-lint for checking Pug/Jade files.

call ale#Set('pug_puglint_options', '')
call ale#Set('pug_puglint_executable', 'pug-lint')
call ale#Set('pug_puglint_use_global', get(g:, 'ale_use_global_executables', 0))

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
    let l:options = ale#Var(a:buffer, 'pug_puglint_options')
    let l:config = s:FindConfig(a:buffer)

    return '%e' . ale#Pad(l:options)
    \   . (!empty(l:config) ? ' -c ' . ale#Escape(l:config) : '')
    \   . ' -r inline %t'
endfunction

call ale#linter#Define('pug', {
\   'name': 'puglint',
\   'executable_callback': ale#node#FindExecutableFunc('pug_puglint', [
\       'node_modules/.bin/pug-lint',
\   ]),
\   'output_stream': 'stderr',
\   'command_callback': 'ale_linters#pug#puglint#GetCommand',
\   'callback': 'ale#handlers#unix#HandleAsError',
\})
