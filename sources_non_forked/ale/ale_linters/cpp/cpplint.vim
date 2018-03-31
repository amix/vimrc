" Author: Dawid Kurek https://github.com/dawikur
" Description: cpplint for cpp files

call ale#Set('cpp_cpplint_executable', 'cpplint')
call ale#Set('cpp_cpplint_options', '')

function! ale_linters#cpp#cpplint#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'cpp_cpplint_executable')
endfunction

function! ale_linters#cpp#cpplint#GetCommand(buffer) abort
    let l:options = ale#Var(a:buffer, 'cpp_cpplint_options')

    return ale#Escape(ale_linters#cpp#cpplint#GetExecutable(a:buffer))
    \   . (!empty(l:options) ? ' ' . l:options : '')
    \   . ' %s'
endfunction

call ale#linter#Define('cpp', {
\   'name': 'cpplint',
\   'output_stream': 'stderr',
\   'executable_callback': 'ale_linters#cpp#cpplint#GetExecutable',
\   'command_callback': 'ale_linters#cpp#cpplint#GetCommand',
\   'callback': 'ale#handlers#cpplint#HandleCppLintFormat',
\   'lint_file': 1,
\})
