" Author: circld <circld1@gmail.com>
" Description: Fixing files with autoflake.

call ale#Set('python_autoflake_executable', 'autoflake')
call ale#Set('python_autoflake_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('python_autoflake_options', '')
call ale#Set('python_autoflake_auto_pipenv', 0)
call ale#Set('python_autoflake_auto_poetry', 0)
call ale#Set('python_autoflake_auto_uv', 0)

function! ale#fixers#autoflake#GetExecutable(buffer) abort
    if (ale#Var(a:buffer, 'python_auto_pipenv') || ale#Var(a:buffer, 'python_autoflake_auto_pipenv'))
    \ && ale#python#PipenvPresent(a:buffer)
        return 'pipenv'
    endif

    if (ale#Var(a:buffer, 'python_auto_poetry') || ale#Var(a:buffer, 'python_autoflake_auto_poetry'))
    \ && ale#python#PoetryPresent(a:buffer)
        return 'poetry'
    endif

    if (ale#Var(a:buffer, 'python_auto_uv') || ale#Var(a:buffer, 'python_autoflake_auto_uv'))
    \ && ale#python#UvPresent(a:buffer)
        return 'uv'
    endif

    return ale#python#FindExecutable(a:buffer, 'python_autoflake', ['autoflake'])
endfunction

function! ale#fixers#autoflake#Fix(buffer) abort
    let l:executable = ale#fixers#autoflake#GetExecutable(a:buffer)

    let l:exec_args = l:executable =~? 'pipenv\|poetry\|uv$'
    \   ? ' run autoflake'
    \   : ''

    let l:options = ale#Var(a:buffer, 'python_autoflake_options')

    return {
    \   'command': ale#Escape(l:executable) . l:exec_args
    \       . (!empty(l:options) ? ' ' . l:options : '')
    \       . ' --in-place '
    \       . ' %t',
    \   'read_temporary_file': 1,
    \}
endfunction
