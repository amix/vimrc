" Author: w0rp <devw0rp@gmail.com>
" Description: A language server for Rust

call ale#Set('rust_rls_executable', 'rls')
call ale#Set('rust_rls_toolchain', 'nightly')

function! ale_linters#rust#rls#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'rust_rls_executable')
endfunction

function! ale_linters#rust#rls#GetCommand(buffer) abort
    let l:executable = ale_linters#rust#rls#GetExecutable(a:buffer)
    let l:toolchain = ale#Var(a:buffer, 'rust_rls_toolchain')

    return ale#Escape(l:executable)
    \   . ' +' . ale#Escape(l:toolchain)
endfunction

function! ale_linters#rust#rls#GetLanguage(buffer) abort
    return 'rust'
endfunction

function! ale_linters#rust#rls#GetProjectRoot(buffer) abort
    let l:cargo_file = ale#path#FindNearestFile(a:buffer, 'Cargo.toml')

    return !empty(l:cargo_file) ? fnamemodify(l:cargo_file, ':h') : ''
endfunction

call ale#linter#Define('rust', {
\   'name': 'rls',
\   'lsp': 'stdio',
\   'executable_callback': 'ale_linters#rust#rls#GetExecutable',
\   'command_callback': 'ale_linters#rust#rls#GetCommand',
\   'language_callback': 'ale_linters#rust#rls#GetLanguage',
\   'project_root_callback': 'ale_linters#rust#rls#GetProjectRoot',
\})
