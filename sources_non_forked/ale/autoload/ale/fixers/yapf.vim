" Author: w0rp <devw0rp@gmail.com>
" Description: Fixing Python files with yapf.

call ale#Set('python_yapf_executable', 'yapf')
call ale#Set('python_yapf_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('python_yapf_auto_pipenv', 0)
call ale#Set('python_yapf_auto_poetry', 0)
call ale#Set('python_yapf_auto_uv', 0)

function! ale#fixers#yapf#GetExecutable(buffer) abort
    if (ale#Var(a:buffer, 'python_auto_pipenv') || ale#Var(a:buffer, 'python_yapf_auto_pipenv'))
    \ && ale#python#PipenvPresent(a:buffer)
        return 'pipenv'
    endif

    if (ale#Var(a:buffer, 'python_auto_poetry') || ale#Var(a:buffer, 'python_yapf_auto_poetry'))
    \ && ale#python#PoetryPresent(a:buffer)
        return 'poetry'
    endif

    if (ale#Var(a:buffer, 'python_auto_uv') || ale#Var(a:buffer, 'python_yapf_auto_uv'))
    \ && ale#python#UvPresent(a:buffer)
        return 'uv'
    endif

    return ale#python#FindExecutable(a:buffer, 'python_yapf', ['yapf'])
endfunction

function! ale#fixers#yapf#Fix(buffer) abort
    let l:executable = ale#fixers#yapf#GetExecutable(a:buffer)

    let l:exec_args = l:executable =~? 'pipenv\|poetry\|uv$'
    \   ? ' run yapf'
    \   : ''

    let l:config = ale#path#FindNearestFile(a:buffer, '.style.yapf')
    let l:config_options = !empty(l:config)
    \   ? ' --no-local-style --style ' . ale#Escape(l:config)
    \   : ''

    return {
    \   'command': ale#Escape(l:executable) . l:exec_args . l:config_options,
    \}
endfunction
