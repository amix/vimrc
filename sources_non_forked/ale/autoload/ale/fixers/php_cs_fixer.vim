" Author: Julien Deniau <julien.deniau@gmail.com>
" Description: Fixing files with php-cs-fixer.

call ale#Set('php_cs_fixer_executable', 'php-cs-fixer')
call ale#Set('php_cs_fixer_use_global', 0)

function! ale#fixers#php_cs_fixer#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'php_cs_fixer', [
    \   'vendor/bin/php-cs-fixer',
    \   'php-cs-fixer'
    \])
endfunction

function! ale#fixers#php_cs_fixer#Fix(buffer) abort
    let l:executable = ale#fixers#php_cs_fixer#GetExecutable(a:buffer)
    return {
    \   'command': ale#Escape(l:executable) . ' fix %t',
    \   'read_temporary_file': 1,
    \}
endfunction



