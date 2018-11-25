" Author: aurieh <me@aurieh.me>
" Description: A language server for dart

call ale#Set('dart_language_server_executable', 'dart_language_server')

function! ale_linters#dart#language_server#GetProjectRoot(buffer) abort
    " Note: pub only looks for pubspec.yaml, there's no point in adding
    " support for pubspec.yml
    let l:pubspec = ale#path#FindNearestFile(a:buffer, 'pubspec.yaml')

    return !empty(l:pubspec) ? fnamemodify(l:pubspec, ':h:h') : ''
endfunction

call ale#linter#Define('dart', {
\   'name': 'language_server',
\   'lsp': 'stdio',
\   'executable_callback': ale#VarFunc('dart_language_server_executable'),
\   'command_callback': 'ale_linters#dart#language_server#GetExecutable',
\   'project_root_callback': 'ale_linters#dart#language_server#GetProjectRoot',
\})
