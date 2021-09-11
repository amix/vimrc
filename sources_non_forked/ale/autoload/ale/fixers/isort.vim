" Author: w0rp <devw0rp@gmail.com>
" Description: Fixing Python imports with isort.

call ale#Set('python_isort_executable', 'isort')
call ale#Set('python_isort_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('python_isort_options', '')
call ale#Set('python_isort_auto_pipenv', 0)
call ale#Set('python_isort_auto_poetry', 0)

function! ale#fixers#isort#GetExecutable(buffer) abort
    if (ale#Var(a:buffer, 'python_auto_pipenv') || ale#Var(a:buffer, 'python_isort_auto_pipenv'))
    \ && ale#python#PipenvPresent(a:buffer)
        return 'pipenv'
    endif

    if (ale#Var(a:buffer, 'python_auto_poetry') || ale#Var(a:buffer, 'python_isort_auto_poetry'))
    \ && ale#python#PoetryPresent(a:buffer)
        return 'poetry'
    endif

    return ale#python#FindExecutable(a:buffer, 'python_isort', ['isort'])
endfunction

function! ale#fixers#isort#Fix(buffer) abort
    let l:executable = ale#fixers#isort#GetExecutable(a:buffer)
    let l:cmd = [ale#Escape(l:executable)]

    if l:executable =~? 'pipenv\|poetry$'
        call extend(l:cmd, ['run', 'isort'])
    endif

    call add(l:cmd, '--filename %s')

    let l:options = ale#Var(a:buffer, 'python_isort_options')

    if !empty(l:options)
        call add(l:cmd, l:options)
    endif

    call add(l:cmd, '-')

    return {
    \   'cwd': '%s:h',
    \   'command': join(l:cmd, ' ')
    \}
endfunction
