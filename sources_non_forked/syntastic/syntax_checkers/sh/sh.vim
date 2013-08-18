"============================================================================
"File:        sh.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Gregor Uhlenheuer <kongo2002 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_sh_sh_checker")
    finish
endif
let g:loaded_syntastic_sh_sh_checker=1

function! s:GetShell()
    if !exists('b:shell') || b:shell == ''
        let b:shell = ''
        let shebang = getbufline(bufnr('%'), 1)[0]
        if len(shebang) > 0
            if match(shebang, 'bash') >= 0
                let b:shell = 'bash'
            elseif match(shebang, 'zsh') >= 0
                let b:shell = 'zsh'
            elseif match(shebang, 'sh') >= 0
                let b:shell = 'sh'
            endif
        endif
        " try to use env variable in case no shebang could be found
        if b:shell == ''
            let b:shell = fnamemodify(expand('$SHELL'), ':t')
        endif
    endif
    return b:shell
endfunction

function! s:ForwardToZshChecker()
    let registry = g:SyntasticRegistry.Instance()
    if registry.checkable('zsh')
        return SyntaxCheckers_zsh_zsh_GetLocList()
    else
        return []
    endif

endfunction


function! s:IsShellValid()
    return len(s:GetShell()) > 0 && executable(s:GetShell())
endfunction


function! SyntaxCheckers_sh_sh_IsAvailable()
    return s:IsShellValid()
endfunction

function! SyntaxCheckers_sh_sh_GetLocList()
    if s:GetShell() == 'zsh'
        return s:ForwardToZshChecker()
    endif

    if !s:IsShellValid()
        return []
    endif

    let makeprg = syntastic#makeprg#build({
        \ 'exe': s:GetShell(),
        \ 'args': '-n',
        \ 'filetype': 'sh',
        \ 'subchecker': 'sh'})

    let errorformat = '%f: line %l: %m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat})
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'sh',
    \ 'name': 'sh'})
