" Author: w0rp <devw0rp@gmail.com>
" Description: Fixing files with autopep8.

call ale#Set('python_autopep8_executable', 'autopep8')
call ale#Set('python_autopep8_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('python_autopep8_options', '')
call ale#Set('python_autopep8_auto_pipenv', 0)
call ale#Set('python_autopep8_auto_poetry', 0)
call ale#Set('python_autopep8_auto_uv', 0)

function! ale#fixers#autopep8#GetExecutable(buffer) abort
    if (ale#Var(a:buffer, 'python_auto_pipenv') || ale#Var(a:buffer, 'python_autopep8_auto_pipenv'))
    \ && ale#python#PipenvPresent(a:buffer)
        return 'pipenv'
    endif

    if (ale#Var(a:buffer, 'python_auto_poetry') || ale#Var(a:buffer, 'python_autopep8_auto_poetry'))
    \ && ale#python#PoetryPresent(a:buffer)
        return 'poetry'
    endif

    if (ale#Var(a:buffer, 'python_auto_uv') || ale#Var(a:buffer, 'python_autopep8_auto_uv'))
    \ && ale#python#UvPresent(a:buffer)
        return 'uv'
    endif

    return ale#python#FindExecutable(a:buffer, 'python_autopep8', ['autopep8'])
endfunction

function! ale#fixers#autopep8#Fix(buffer) abort
    let l:executable = ale#fixers#autopep8#GetExecutable(a:buffer)

    let l:exec_args = l:executable =~? 'pipenv\|poetry\|uv$'
    \   ? ' run autopep8'
    \   : ''

    let l:options = ale#Var(a:buffer, 'python_autopep8_options')

    return {
    \   'command': ale#Escape(l:executable) . l:exec_args
    \       . (!empty(l:options) ? ' ' . l:options : '')
    \       . ' -',
    \}
endfunction
