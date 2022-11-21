" Author: w0rp <devw0rp@gmail.com>
" Description: tsserver integration for ALE

call ale#Set('html_angular_executable', 'ngserver')
call ale#Set('html_angular_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale_linters#html#angular#GetProjectRoot(buffer) abort
    return ale#path#Dirname(
    \   ale#path#FindNearestDirectory(a:buffer, 'node_modules')
    \)
endfunction

function! ale_linters#html#angular#GetExecutable(buffer) abort
    return 'node'
endfunction

function! ale_linters#html#angular#GetCommand(buffer) abort
    let l:language_service_dir = ale#path#Simplify(
    \   ale#path#FindNearestDirectory(
    \       a:buffer,
    \       'node_modules/@angular/language-service'
    \   )
    \)

    if empty(l:language_service_dir)
        return ''
    endif

    let l:language_service_dir = fnamemodify(l:language_service_dir, ':h')
    let l:typescript_dir = ale#path#Simplify(
    \   fnamemodify(l:language_service_dir, ':h:h')
    \   . '/typescript'
    \)
    let l:script = ale#path#FindExecutable(a:buffer, 'html_angular', [
    \   'node_modules/@angular/language-server/bin/ngserver',
    \   'node_modules/@angular/language-server/index.js',
    \])

    if !filereadable(l:script)
        return ''
    endif

    return ale#Escape('node') . ' ' . ale#Escape(l:script)
    \ . ' --ngProbeLocations ' . ale#Escape(l:language_service_dir)
    \ . ' --tsProbeLocations ' . ale#Escape(l:typescript_dir)
    \ . ' --stdio'
endfunction

call ale#linter#Define('html', {
\   'name': 'angular',
\   'aliases': ['angular-language-server'],
\   'lsp': 'stdio',
\   'executable': function('ale_linters#html#angular#GetExecutable'),
\   'command': function('ale_linters#html#angular#GetCommand'),
\   'project_root': function('ale_linters#html#angular#GetProjectRoot'),
\})
