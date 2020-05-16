" Author: Kenneth Benzie <k.benzie83@gmail.com>
" Description: cmakelint for cmake files

let g:ale_cmake_cmakelint_executable =
\   get(g:, 'ale_cmake_cmakelint_executable', 'cmakelint')

let g:ale_cmake_cmakelint_options =
\   get(g:, 'ale_cmake_cmakelint_options', '')

function! ale_linters#cmake#cmakelint#Executable(buffer) abort
    return ale#Var(a:buffer, 'cmake_cmakelint_executable')
endfunction

function! ale_linters#cmake#cmakelint#Command(buffer) abort
    return ale_linters#cmake#cmakelint#Executable(a:buffer)
    \   . ' ' . ale#Var(a:buffer, 'cmake_cmakelint_options') . ' %t'
endfunction

call ale#linter#Define('cmake', {
\   'name': 'cmakelint',
\   'executable': function('ale_linters#cmake#cmakelint#Executable'),
\   'command': function('ale_linters#cmake#cmakelint#Command'),
\   'callback': 'ale#handlers#unix#HandleAsWarning',
\})
