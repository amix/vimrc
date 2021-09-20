" Author: w0rp <devw0rp@gmail.com>
" Description: Fixing Python imports with isort.

call ale#Set('python_isort_executable', 'isort')
call ale#Set('python_isort_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('python_isort_options', '')
call ale#Set('python_isort_auto_pipenv', 0)

function! ale#fixers#isort#GetExecutable(buffer) abort
    if (ale#Var(a:buffer, 'python_auto_pipenv') || ale#Var(a:buffer, 'python_isort_auto_pipenv'))
    \ && ale#python#PipenvPresent(a:buffer)
        return 'pipenv'
    endif

    return ale#python#FindExecutable(a:buffer, 'python_isort', ['isort'])
endfunction

function! ale#fixers#isort#Fix(buffer) abort
    let l:options = ale#Var(a:buffer, 'python_isort_options')
    let l:executable = ale#fixers#isort#GetExecutable(a:buffer)
    let l:exec_args = l:executable =~? 'pipenv$'
    \   ? ' run isort'
    \   : ''

    if !executable(l:executable) && l:executable isnot# 'pipenv'
        return 0
    endif

    return {
    \   'cwd': '%s:h',
    \   'command': ale#Escape(l:executable) . l:exec_args
    \   . ale#Pad('--filename %s')
    \   . (!empty(l:options) ? ' ' . l:options : '') . ' -',
    \}
endfunction
