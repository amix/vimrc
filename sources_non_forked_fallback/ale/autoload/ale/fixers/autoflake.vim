" Author: circld <circld1@gmail.com>
" Description: Fixing files with autoflake.

call ale#Set('python_autoflake_executable', 'autoflake')
call ale#Set('python_autoflake_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('python_autoflake_options', '')

function! ale#fixers#autoflake#Fix(buffer) abort
    let l:executable = ale#python#FindExecutable(
    \   a:buffer,
    \   'python_autoflake',
    \   ['autoflake'],
    \)

    if !executable(l:executable)
        return 0
    endif

    let l:options = ale#Var(a:buffer, 'python_autoflake_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . (!empty(l:options) ? ' ' . l:options : '')
    \       . ' --in-place '
    \       . ' %t',
    \   'read_temporary_file': 1,
    \}
endfunction
