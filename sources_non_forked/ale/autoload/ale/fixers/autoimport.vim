" Author: lyz-code
" Description: Fixing Python imports with autoimport.

call ale#Set('python_autoimport_executable', 'autoimport')
call ale#Set('python_autoimport_options', '')
call ale#Set('python_autoimport_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('python_autoimport_auto_pipenv', 0)
call ale#Set('python_autoimport_auto_poetry', 0)
call ale#Set('python_autoimport_auto_uv', 0)

function! ale#fixers#autoimport#GetExecutable(buffer) abort
    if (ale#Var(a:buffer, 'python_auto_pipenv') || ale#Var(a:buffer, 'python_autoimport_auto_pipenv'))
    \ && ale#python#PipenvPresent(a:buffer)
        return 'pipenv'
    endif

    if (ale#Var(a:buffer, 'python_auto_poetry') || ale#Var(a:buffer, 'python_autoimport_auto_poetry'))
    \ && ale#python#PoetryPresent(a:buffer)
        return 'poetry'
    endif

    if (ale#Var(a:buffer, 'python_auto_uv') || ale#Var(a:buffer, 'python_autoimport_auto_uv'))
    \ && ale#python#UvPresent(a:buffer)
        return 'uv'
    endif

    return ale#python#FindExecutable(a:buffer, 'python_autoimport', ['autoimport'])
endfunction

function! ale#fixers#autoimport#Fix(buffer) abort
    let l:executable = ale#fixers#autoimport#GetExecutable(a:buffer)

    let l:exec_args = l:executable =~? 'pipenv\|poetry\|uv$'
    \   ? ' run autoimport'
    \   : ''

    let l:options = ale#Var(a:buffer, 'python_autoimport_options')

    return {
    \   'cwd': '%s:h',
    \   'command': ale#Escape(l:executable) . l:exec_args
    \       . (!empty(l:options) ? ' ' . l:options : '')
    \       . ' -',
    \}
endfunction
