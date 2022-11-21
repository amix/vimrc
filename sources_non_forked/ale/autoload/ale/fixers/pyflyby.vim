" Author: infokiller <joweill@icloud.com>
" Description: Tidy imports using pyflyby's tidy-import script
" https://github.com/deshaw/pyflyby

call ale#Set('python_pyflyby_executable', 'tidy-imports')
call ale#Set('python_pyflyby_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('python_pyflyby_options', '')
call ale#Set('python_pyflyby_auto_pipenv', 0)
call ale#Set('python_pyflyby_auto_poetry', 0)

function! ale#fixers#pyflyby#GetExecutable(buffer) abort
    if (ale#Var(a:buffer, 'python_auto_pipenv') || ale#Var(a:buffer, 'python_pyflyby_auto_pipenv'))
    \ && ale#python#PipenvPresent(a:buffer)
        return 'pipenv'
    endif

    if (ale#Var(a:buffer, 'python_auto_poetry') || ale#Var(a:buffer, 'python_pyflyby_auto_poetry'))
    \ && ale#python#PoetryPresent(a:buffer)
        return 'poetry'
    endif

    return ale#python#FindExecutable(a:buffer, 'python_pyflyby', ['tidy-imports'])
endfunction

function! ale#fixers#pyflyby#Fix(buffer) abort
    " let l:executable = ale#fixers#pyflyby#GetExecutable(a:buffer)
    let l:executable = ale#fixers#pyflyby#GetExecutable(a:buffer)
    let l:cmd = [ale#Escape(l:executable)]

    if l:executable =~? 'pipenv\|poetry$'
        call extend(l:cmd, ['run', 'tidy-imports'])
    endif

    let l:options = ale#Var(a:buffer, 'python_pyflyby_options')

    if !empty(l:options)
        call add(l:cmd, l:options)
    endif

    return {'command': join(l:cmd, ' ')}
endfunction
