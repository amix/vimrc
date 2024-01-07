" Author: Yen3 <yen3rc@gmail.com>
" Description: A language server for haskell
"              The file is based on hie.vim (author: Luxed
"              <devildead13@gmail.com>).  It search more project root files.
"
call ale#Set('haskell_hls_executable', 'haskell-language-server-wrapper')
call ale#Set('haskell_hls_config', {})

function! ale_linters#haskell#hls#FindRootFile(buffer) abort
    let l:serach_root_files = [
    \ 'stack.yaml',
    \ 'cabal.project',
    \ 'package.yaml',
    \ 'hie.yaml'
    \ ]

    for l:path in ale#path#Upwards(expand('#' . a:buffer . ':p:h'))
        for l:root_file in l:serach_root_files
            if filereadable(l:path . '/' . l:root_file)
                " Add on / so fnamemodify(..., ':h') below keeps the path.
                return l:path . '/'
            endif
        endfor
    endfor

    return ''
endfunction

function! ale_linters#haskell#hls#GetProjectRoot(buffer) abort
    " Search for the project file first
    let l:project_file = ale_linters#haskell#hls#FindRootFile(a:buffer)

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

function! ale_linters#haskell#hls#GetCommand(buffer) abort
    let l:executable = ale#Var(a:buffer, 'haskell_hls_executable')

    return ale#handlers#haskell_stack#EscapeExecutable(l:executable,
    \ 'haskell-language-server-wrapper')
    \ . ' --lsp'
endfunction

call ale#linter#Define('haskell', {
\   'name': 'hls',
\   'lsp': 'stdio',
\   'command': function('ale_linters#haskell#hls#GetCommand'),
\   'executable': {b -> ale#Var(b, 'haskell_hls_executable')},
\   'project_root': function('ale_linters#haskell#hls#GetProjectRoot'),
\   'lsp_config': {b -> ale#Var(b, 'haskell_hls_config')},
\})
