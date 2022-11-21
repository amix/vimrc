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


call ale#Set('go_go111module', '')

" Return a string setting Go-specific environment variables
function! ale#go#EnvString(buffer) abort
    let l:env = ''

    " GO111MODULE - turn go modules behavior on/off
    let l:go111module = ale#Var(a:buffer, 'go_go111module')

    if !empty(l:go111module)
        let l:env = ale#Env('GO111MODULE', l:go111module) . l:env
    endif

    return l:env
endfunction

function! ale#go#GetGoPathExecutable(suffix) abort
    let l:prefix = $GOPATH

    if !empty($GOPATH)
        let l:prefix = $GOPATH
    elseif has('win32')
        let l:prefix = $USERPROFILE . '/go'
    else
        let l:prefix = $HOME . '/go'
    endif

    return ale#path#Simplify(l:prefix . '/' . a:suffix)
endfunction
