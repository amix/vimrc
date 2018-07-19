" Author: w0rp <devw0rp@gmail.com>
" Description: Fixing Python imports with isort.

call ale#Set('python_isort_executable', 'isort')
call ale#Set('python_isort_options', '')
call ale#Set('python_isort_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale#fixers#isort#Fix(buffer) abort
    let l:options = ale#Var(a:buffer, 'python_isort_options')

    let l:executable = ale#python#FindExecutable(
    \   a:buffer,
    \   'python_isort',
    \   ['isort'],
    \)

    if !executable(l:executable)
        return 0
    endif

    return {
    \   'command': ale#path#BufferCdString(a:buffer)
    \   .   ale#Escape(l:executable) . (!empty(l:options) ? ' ' . l:options : '') . ' -',
    \}
endfunction
