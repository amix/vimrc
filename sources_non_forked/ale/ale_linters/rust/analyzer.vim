" Author: Jon Gjengset <jon@thesquareplanet.com>
" Description: The next generation language server for Rust

call ale#Set('rust_analyzer_executable', 'rust-analyzer')
call ale#Set('rust_analyzer_config', {})

function! ale_linters#rust#analyzer#GetCommand(buffer) abort
    return '%e'
endfunction

function! ale_linters#rust#analyzer#GetProjectRoot(buffer) abort
    " Try to find nearest Cargo.toml for cargo projects
    let l:cargo_file = ale#path#FindNearestFile(a:buffer, 'Cargo.toml')

    if !empty(l:cargo_file)
        return fnamemodify(l:cargo_file, ':h')
    endif

    " Try to find nearest rust-project.json for non-cargo projects
    let l:rust_project = ale#path#FindNearestFile(a:buffer, 'rust-project.json')

    if !empty(l:rust_project)
        return fnamemodify(l:rust_project, ':h')
    endif

    return ''
endfunction

call ale#linter#Define('rust', {
\   'name': 'analyzer',
\   'aliases': ['rust_analyzer'],
\   'lsp': 'stdio',
\   'initialization_options': {b -> ale#Var(b, 'rust_analyzer_config')},
\   'executable': {b -> ale#Var(b, 'rust_analyzer_executable')},
\   'command': function('ale_linters#rust#analyzer#GetCommand'),
\   'project_root': function('ale_linters#rust#analyzer#GetProjectRoot'),
\})
