" Author: Luxed <devildead13@gmail.com>
" Description: A language server for Haskell

call ale#Set('haskell_hie_executable', 'hie')

function! ale_linters#haskell#hie#GetProjectRoot(buffer) abort
    " Search for the stack file first
    let l:project_file = ale#path#FindNearestFile(a:buffer, 'stack.yaml')

    " If it's empty, search for the cabal file
    if empty(l:project_file)
        " Search all of the paths except for the root filesystem path.
        let l:paths = join(
        \   ale#path#Upwards(expand('#' . a:buffer . ':p:h'))[:-2],
        \   ','
        \)
        let l:project_file = globpath(l:paths, '*.cabal')
    endif

    " If we still can't find one, use the current file.
    if empty(l:project_file)
        let l:project_file = expand('#' . a:buffer . ':p')
    endif

    return fnamemodify(l:project_file, ':h')
endfunction

function! ale_linters#haskell#hie#GetCommand(buffer) abort
    let l:executable = ale#Var(a:buffer, 'haskell_hie_executable')

    return ale#handlers#haskell_stack#EscapeExecutable(l:executable, 'hie')
    \   . ' --lsp'
endfunction

call ale#linter#Define('haskell', {
\   'name': 'hie',
\   'lsp': 'stdio',
\   'command': function('ale_linters#haskell#hie#GetCommand'),
\   'executable': {b -> ale#Var(b, 'haskell_hie_executable')},
\   'project_root': function('ale_linters#haskell#hie#GetProjectRoot'),
\})
