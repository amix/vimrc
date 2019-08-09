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
    let l:cd_string = ale#Var(a:buffer, 'python_black_change_directory')
    \   ? ale#path#BufferCdString(a:buffer)
    \   : ''

    let l:executable = ale#fixers#black#GetExecutable(a:buffer)

    let l:exec_args = l:executable =~? 'pipenv$'
    \   ? ' run black'
    \   : ''

    let l:options = ale#Var(a:buffer, 'python_black_options')

    return {
    \   'command': l:cd_string . ale#Escape(l:executable) . l:exec_args
    \       . (!empty(l:options) ? ' ' . l:options : '')
    \       . ' -',
    \}
endfunction
