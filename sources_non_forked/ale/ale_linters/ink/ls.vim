" Author: Andreww Hayworth <ahayworth@gmail.com>
" Description: Integrate ALE with ink-language-server

call ale#Set('ink_ls_executable', 'ink-language-server')
call ale#Set('ink_ls_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('ink_ls_initialization_options', {})

function! ale_linters#ink#ls#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'ink_ls', [
    \   'ink-language-server',
    \   'node_modules/.bin/ink-language-server',
    \])
endfunction

function! ale_linters#ink#ls#GetCommand(buffer) abort
    let l:executable = ale_linters#ink#ls#GetExecutable(a:buffer)

    return ale#Escape(l:executable) . ' --stdio'
endfunction

function! ale_linters#ink#ls#FindProjectRoot(buffer) abort
    let l:main_file = get(ale#Var(a:buffer, 'ink_ls_initialization_options'), 'mainStoryPath', 'main.ink')
    let l:config = ale#path#ResolveLocalPath(a:buffer, l:main_file, expand('#' . a:buffer . ':p'))

    return ale#path#Dirname(l:config)
endfunction

call ale#linter#Define('ink', {
\   'name': 'ink-language-server',
\   'lsp': 'stdio',
\   'executable': function('ale_linters#ink#ls#GetExecutable'),
\   'command': function('ale_linters#ink#ls#GetCommand'),
\   'project_root': function('ale_linters#ink#ls#FindProjectRoot'),
\   'initialization_options': {b -> ale#Var(b, 'ink_ls_initialization_options')},
\})
