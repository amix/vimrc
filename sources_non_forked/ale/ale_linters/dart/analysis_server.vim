" Author: Nelson Yeung <nelsyeung@gmail.com>
" Description: Check Dart files with dart analysis server LSP

call ale#Set('dart_analysis_server_enable_language_server', 1)
call ale#Set('dart_analysis_server_executable', 'dart')

function! ale_linters#dart#analysis_server#GetProjectRoot(buffer) abort
    " Note: pub only looks for pubspec.yaml, there's no point in adding
    " support for pubspec.yml
    let l:pubspec = ale#path#FindNearestFile(a:buffer, 'pubspec.yaml')

    return !empty(l:pubspec) ? fnamemodify(l:pubspec, ':h:h') : '.'
endfunction

function! ale_linters#dart#analysis_server#GetCommand(buffer) abort
    let l:language_server = ale#Var(a:buffer, 'dart_analysis_server_enable_language_server')
    let l:executable = ale#Var(a:buffer, 'dart_analysis_server_executable')
    let l:dart = resolve(exepath(l:executable))
    let l:output = '%e '
    \   . fnamemodify(l:dart, ':h') . '/snapshots/analysis_server.dart.snapshot'
    \   . ' --lsp'

    " Enable new language-server command
    if l:language_server == 1
        let l:output = '%e language-server --protocol=lsp'
    endif

    return l:output
endfunction

call ale#linter#Define('dart', {
\   'name': 'analysis_server',
\   'lsp': 'stdio',
\   'executable': {b -> ale#Var(b, 'dart_analysis_server_executable')},
\   'command': function('ale_linters#dart#analysis_server#GetCommand'),
\   'project_root': function('ale_linters#dart#analysis_server#GetProjectRoot'),
\})
