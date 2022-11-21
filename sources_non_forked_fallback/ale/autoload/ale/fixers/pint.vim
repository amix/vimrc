" Author: Michael Dyrynda <michael@dyrynda.com.au>
" Description: Fixing files with Laravel Pint.

call ale#Set('php_pint_executable', 'pint')
call ale#Set('php_pint_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('php_pint_options', '')

function! ale#fixers#pint#GetExecutable(buffer) abort
    return ale#path#FindExecutable(a:buffer, 'php_pint', [
    \   'vendor/bin/pint',
    \   'pint'
    \])
endfunction

function! ale#fixers#pint#Fix(buffer) abort
    let l:executable = ale#fixers#pint#GetExecutable(a:buffer)

    return {
    \   'command': ale#Escape(l:executable)
    \       . ' ' . ale#Var(a:buffer, 'php_pint_options')
    \       . ' %t',
    \   'read_temporary_file': 1,
    \}
endfunction

