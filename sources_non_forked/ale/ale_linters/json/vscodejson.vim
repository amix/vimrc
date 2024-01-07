" Author: Dalius Dobravolskas <dalius.dobravolskas@gmail.com>
" Description: VSCode json language server

call ale#Set('json_vscodejson_executable', '<auto>')

function! ale_linters#json#vscodejson#GetExecutable(buffer) abort
    let l:executable = ale#Var(a:buffer, 'json_vscodejson_executable')

    if l:executable is# '<auto>'
        if ale#engine#IsExecutable(a:buffer, 'vscode-json-languageserver')
            let l:executable = 'vscode-json-languageserver'
        else
            let l:executable = 'vscode-json-language-server'
        endif
    endif

    return l:executable
endfunction

function! ale_linters#json#vscodejson#GetProjectRoot(buffer) abort
    let l:git_path = ale#path#FindNearestDirectory(a:buffer, '.git')

    return !empty(l:git_path) ? fnamemodify(l:git_path, ':h:h') : ''
endfunction

call ale#linter#Define('json', {
\   'name': 'vscodejson',
\   'lsp': 'stdio',
\   'executable': function('ale_linters#json#vscodejson#GetExecutable'),
\   'command': '%e --stdio',
\   'project_root': function('ale_linters#json#vscodejson#GetProjectRoot'),
\})
