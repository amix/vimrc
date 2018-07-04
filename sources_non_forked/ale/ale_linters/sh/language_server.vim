" Author: Christian Höltje (https://docwhat.org/)
" Description: BASH Language server integration for ALE
scriptencoding utf-8

call ale#Set('sh_language_server_executable', 'bash-language-server')
call ale#Set('sh_language_server_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale_linters#sh#language_server#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'sh_language_server', [
    \   'node_modules/.bin/bash-language-server',
    \])
endfunction

function! ale_linters#sh#language_server#GetCommand(buffer) abort
    let l:exe = ale#Escape(ale_linters#sh#language_server#GetExecutable(a:buffer))

    return l:exe . ' start'
endfunction

function! ale_linters#sh#language_server#GetProjectRoot(buffer) abort
    let l:git_path = ale#path#FindNearestDirectory(a:buffer, '.git')

    return !empty(l:git_path) ? fnamemodify(l:git_path, ':h:h') : ''
endfunction

call ale#linter#Define('sh', {
\   'name': 'language_server',
\   'lsp': 'stdio',
\   'executable_callback': 'ale_linters#sh#language_server#GetExecutable',
\   'command_callback': 'ale_linters#sh#language_server#GetCommand',
\   'language': 'sh',
\   'project_root_callback': 'ale_linters#sh#language_server#GetProjectRoot',
\})
