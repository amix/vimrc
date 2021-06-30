" Author: Jon Gjengset <jon@thesquareplanet.com>
" Description: The next generation language server for Rust

call ale#Set('rust_analyzer_executable', 'rust-analyzer')
call ale#Set('rust_analyzer_config', {})

function! ale_linters#rust#analyzer#GetCommand(buffer) abort
    return '%e'
endfunction

function! ale_linters#rust#analyzer#GetProjectRoot(buffer) abort
    let l:cargo_file = ale#path#FindNearestFile(a:buffer, 'Cargo.toml')

    return !empty(l:cargo_file) ? fnamemodify(l:cargo_file, ':h') : ''
endfunction

call ale#linter#Define('rust', {
\   'name': 'analyzer',
\   'lsp': 'stdio',
\   'initialization_options': {b -> ale#Var(b, 'rust_analyzer_config')},
\   'executable': {b -> ale#Var(b, 'rust_analyzer_executable')},
\   'command': function('ale_linters#rust#analyzer#GetCommand'),
\   'project_root': function('ale_linters#rust#analyzer#GetProjectRoot'),
\})
