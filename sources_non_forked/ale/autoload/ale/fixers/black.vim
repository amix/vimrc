" Author: w0rp <devw0rp@gmail.com>
" Description: Fixing Python files with black.
"
call ale#Set('python_black_executable', 'black')
call ale#Set('python_black_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('python_black_options', '')
call ale#Set('python_black_auto_pipenv', 0)
call ale#Set('python_black_change_directory', 1)

function! ale#fixers#black#GetExecutable(buffer) abort
    if (ale#Var(a:buffer, 'python_auto_pipenv') || ale#Var(a:buffer, 'python_black_auto_pipenv'))
    \ && ale#python#PipenvPresent(a:buffer)
        return 'pipenv'
    endif

    return ale#python#FindExecutable(a:buffer, 'python_black', ['black'])
endfunction

function! ale#fixers#black#Fix(buffer) abort
    let l:executable = ale#fixers#black#GetExecutable(a:buffer)
    let l:exec_args = l:executable =~? 'pipenv$'
    \   ? ' run black'
    \   : ''
    let l:options = ale#Var(a:buffer, 'python_black_options')

    if expand('#' . a:buffer . ':e') is? 'pyi'
        let l:options .= '--pyi'
    endif

    let l:result = {
    \   'command': ale#Escape(l:executable) . l:exec_args
    \       . (!empty(l:options) ? ' ' . l:options : '')
    \       . ' -',
    \}

    if ale#Var(a:buffer, 'python_black_change_directory')
        let l:result.cwd = '%s:h'
    endif

    return l:result
endfunction
