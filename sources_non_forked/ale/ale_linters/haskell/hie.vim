" Author: Luxed <devildead13@gmail.com>
" Description: A language server for Haskell

call ale#Set('haskell_hie_executable', 'hie')

function! ale_linters#haskell#hie#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'haskell_hie_executable')
endfunction

function! ale_linters#haskell#hie#GetProjectRoot(buffer) abort
    " Search for the stack file first
    let l:project_file = ale#path#FindNearestFile(a:buffer, 'stack.yaml')

    " If it's empty, search for the cabal file
    if empty(l:project_file)
        let l:cabal_file = fnamemodify(bufname(a:buffer), ':p:h')
        let l:paths = ''

        while empty(matchstr(l:cabal_file, '^\(\/\|\(\w:\\\)\)$'))
            let l:cabal_file = fnamemodify(l:cabal_file, ':h')
            let l:paths = l:paths . l:cabal_file . ','
        endwhile

        let l:project_file = globpath(l:paths, '*.cabal')
    endif

    " Either extract the project directory or take the current working
    " directory
    if !empty(l:project_file)
        let l:project_file = fnamemodify(l:project_file, ':h')
    else
        let l:project_file = expand('#' . a:buffer . ':p:h')
    endif

    return l:project_file
endfunction

call ale#linter#Define('haskell', {
\   'name': 'hie',
\   'lsp': 'stdio',
\   'command': '%e --lsp',
\   'executable_callback': 'ale_linters#haskell#hie#GetExecutable',
\   'project_root_callback': 'ale_linters#haskell#hie#GetProjectRoot',
\})
