" Author: w0rp <devw0rp@gmail.com>
" Description: Fixing Python files with black.
"
call ale#Set('python_black_executable', 'black')
call ale#Set('python_black_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('python_black_options', '')

function! ale#fixers#black#Fix(buffer) abort
    let l:executable = ale#python#FindExecutable(
    \   a:buffer,
    \   'python_black',
    \   ['black'],
    \)

    if !executable(l:executable)
        return 0
    endif

    let l:options = ale#Var(a:buffer, 'python_black_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . (!empty(l:options) ? ' ' . l:options : '')
    \       . ' -',
    \}
endfunction
