" Author: Horacio Sanson https://github.com/hsanson
" Description: Functions for integrating with Go tools

" Find the nearest dir listed in GOPATH and assume it the root of the go
" project.
function! ale#go#FindProjectRoot(buffer) abort
    let l:sep = has('win32') ? ';' : ':'

    let l:filename = ale#path#Simplify(expand('#' . a:buffer . ':p'))

    for l:name in split($GOPATH, l:sep)
        let l:path_dir = ale#path#Simplify(l:name)

        " Use the directory from GOPATH if the current filename starts with it.
        if l:filename[: len(l:path_dir) - 1] is? l:path_dir
            return l:path_dir
        endif
    endfor

    let l:default_go_path = ale#path#Simplify(expand('~/go'))

    if isdirectory(l:default_go_path)
        return l:default_go_path
    endif

    return ''
endfunction
