" Author: jake <me@jake.computer>
" Description: Fixing Python imports with reorder-python-imports.

call ale#Set('python_reorder_python_imports_executable', 'reorder-python-imports')
call ale#Set('python_reorder_python_imports_options', '')
call ale#Set('python_reorder_python_imports_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('python_reorder_python_imports_auto_pipenv', 0)
call ale#Set('python_reorder_python_imports_auto_poetry', 0)
call ale#Set('python_reorder_python_imports_auto_uv', 0)

function! ale#fixers#reorder_python_imports#GetExecutable(buffer) abort
    if (ale#Var(a:buffer, 'python_auto_pipenv') || ale#Var(a:buffer, 'python_reorder_python_imports_auto_pipenv'))
    \ && ale#python#PipenvPresent(a:buffer)
        return 'pipenv'
    endif

    if (ale#Var(a:buffer, 'python_auto_poetry') || ale#Var(a:buffer, 'python_reorder_python_imports_auto_poetry'))
    \ && ale#python#PoetryPresent(a:buffer)
        return 'poetry'
    endif

    if (ale#Var(a:buffer, 'python_auto_uv') || ale#Var(a:buffer, 'python_reorder_python_imports_auto_uv'))
    \ && ale#python#UvPresent(a:buffer)
        return 'uv'
    endif

    return ale#python#FindExecutable(a:buffer, 'python_reorder_python_imports', ['reorder-python-imports'])
endfunction

function! ale#fixers#reorder_python_imports#Fix(buffer) abort
    let l:executable = ale#fixers#reorder_python_imports#GetExecutable(a:buffer)

    let l:exec_args = l:executable =~? 'pipenv\|poetry\|uv$'
    \   ? ' run reorder-python-imports'
    \   : ''

    let l:options = ale#Var(a:buffer, 'python_reorder_python_imports_options')

    return {
    \   'command': ale#Escape(l:executable) . l:exec_args
    \       . (!empty(l:options) ? ' ' . l:options : '') . ' -',
    \}
endfunction
