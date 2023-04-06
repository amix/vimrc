" Author: aurieh <me@aurieh.me>
" Description: A language server for Python

call ale#Set('python_pylsp_executable', 'pylsp')
call ale#Set('python_pylsp_options', '')
call ale#Set('python_pylsp_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('python_pylsp_auto_pipenv', 0)
call ale#Set('python_pylsp_auto_poetry', 0)
call ale#Set('python_pylsp_config', {})

function! ale_linters#python#pylsp#GetExecutable(buffer) abort
    if (ale#Var(a:buffer, 'python_auto_pipenv') || ale#Var(a:buffer, 'python_pylsp_auto_pipenv'))
    \ && ale#python#PipenvPresent(a:buffer)
        return 'pipenv'
    endif

    if (ale#Var(a:buffer, 'python_auto_poetry') || ale#Var(a:buffer, 'python_pylsp_auto_poetry'))
    \ && ale#python#PoetryPresent(a:buffer)
        return 'poetry'
    endif

    return ale#python#FindExecutable(a:buffer, 'python_pylsp', ['pylsp'])
endfunction

" Force the cwd of the server to be the same as the project root to
" fix issues with treating local files matching first or third party library
" names being imported incorrectly.
function! ale_linters#python#pylsp#GetCwd(buffer) abort
    let l:fake_linter = {
    \   'name': 'pylsp',
    \   'project_root': function('ale#python#FindProjectRoot'),
    \}
    let l:root = ale#lsp_linter#FindProjectRoot(a:buffer, l:fake_linter)

    return !empty(l:root) ? l:root : v:null
endfunction

function! ale_linters#python#pylsp#GetCommand(buffer) abort
    let l:executable = ale_linters#python#pylsp#GetExecutable(a:buffer)
    let l:exec_args = l:executable =~? 'pipenv\|poetry$'
    \   ? ' run pylsp'
    \   : ''
    let l:env_string = ''

    if ale#Var(a:buffer, 'python_auto_virtualenv')
        let l:env_string = ale#python#AutoVirtualenvEnvString(a:buffer)
    endif

    return l:env_string . ale#Escape(l:executable) . l:exec_args . ale#Pad(ale#Var(a:buffer, 'python_pylsp_options'))
endfunction

call ale#linter#Define('python', {
\   'name': 'pylsp',
\   'lsp': 'stdio',
\   'executable': function('ale_linters#python#pylsp#GetExecutable'),
\   'cwd': function('ale_linters#python#pylsp#GetCwd'),
\   'command': function('ale_linters#python#pylsp#GetCommand'),
\   'project_root': function('ale#python#FindProjectRoot'),
\   'completion_filter': 'ale#completion#python#CompletionItemFilter',
\   'lsp_config': {b -> ale#Var(b, 'python_pylsp_config')},
\})
