" Author: notomo <notomo.motono@gmail.com>
" Description: Fixing files with phpcbf.

call ale#Set('php_phpcbf_standard', '')
call ale#Set('php_phpcbf_options', '')
call ale#Set('php_phpcbf_executable', 'phpcbf')
call ale#Set('php_phpcbf_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale#fixers#phpcbf#GetExecutable(buffer) abort
    return ale#path#FindExecutable(a:buffer, 'php_phpcbf', [
    \   'vendor/bin/phpcbf',
    \   'phpcbf'
    \])
endfunction

function! ale#fixers#phpcbf#Fix(buffer) abort
    let l:executable = ale#fixers#phpcbf#GetExecutable(a:buffer)
    let l:standard = ale#Var(a:buffer, 'php_phpcbf_standard')
    let l:standard_option = !empty(l:standard)
    \   ? '--standard=' . l:standard
    \   : ''

    return {
    \   'command': ale#Escape(l:executable) . ' --stdin-path=%s ' . l:standard_option . ale#Pad(ale#Var(a:buffer, 'php_phpcbf_options')) . ' -'
    \}
endfunction
