" Author: dsifford <dereksifford@gmail.com>
" Description: A performant type-checker supporting LSP for Python 3 created by Facebook

call ale#Set('python_pyre_executable', 'pyre')
call ale#Set('python_pyre_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale_linters#python#pyre#GetExecutable(buffer) abort
    return ale#python#FindExecutable(a:buffer, 'python_pyre', ['pyre'])
endfunction

function! ale_linters#python#pyre#GetCommand(buffer) abort
    let l:executable = ale_linters#python#pyre#GetExecutable(a:buffer)

    let l:exec_args = l:executable =~? 'pipenv$'
    \   ? ' run pyre persistent'
    \   : ' persistent'

    return ale#Escape(l:executable) . l:exec_args
endfunction

call ale#linter#Define('python', {
\   'name': 'pyre',
\   'lsp': 'stdio',
\   'executable_callback': 'ale_linters#python#pyre#GetExecutable',
\   'command_callback': 'ale_linters#python#pyre#GetCommand',
\   'project_root_callback': 'ale#python#FindProjectRoot',
\   'completion_filter': 'ale#completion#python#CompletionItemFilter',
\})
