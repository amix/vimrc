scriptencoding utf-8
" Author: Ye Jingchen <ye.jingchen@gmail.com>
" Description: Utilities for ccls

function! ale#handlers#ccls#GetProjectRoot(buffer) abort
    " Try to find ccls configuration files first.
    let l:config = ale#path#FindNearestFile(a:buffer, '.ccls-root')

    if empty(l:config)
        let l:config = ale#path#FindNearestFile(a:buffer, '.ccls')
    endif

    if !empty(l:config)
        return fnamemodify(l:config, ':h')
    endif

    " Fall back on default project root detection.
    return ale#c#FindProjectRoot(a:buffer)
endfunction

function! ale#handlers#ccls#GetInitOpts(buffer, init_options_var) abort
    let l:build_dir = ale#c#GetBuildDirectory(a:buffer)
    let l:init_options = empty(l:build_dir) ? {} : {'compilationDatabaseDirectory': l:build_dir}

    return extend(l:init_options, ale#Var(a:buffer, a:init_options_var))
endfunction
