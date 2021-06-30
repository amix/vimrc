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

<<<<<<< HEAD
    let l:executable = ale#fixers#isort#GetExecutable(a:buffer)

    let l:exec_args = l:executable =~? 'pipenv$'
    \   ? ' run isort'
    \   : ''

=======
>>>>>>> 1cca3b1df2973096bb9526a0d79c7b93c04e66b3
    if !executable(l:executable) && l:executable isnot# 'pipenv'
        return 0
    endif

    return {
<<<<<<< HEAD
    \   'command': ale#path#BufferCdString(a:buffer)
    \   . ale#Escape(l:executable) . l:exec_args
=======
    \   'cwd': '%s:h',
    \   'command': ale#Escape(l:executable) . l:exec_args
>>>>>>> 1cca3b1df2973096bb9526a0d79c7b93c04e66b3
    \   . (!empty(l:options) ? ' ' . l:options : '') . ' -',
    \}
endfunction
