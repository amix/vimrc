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
let g:loaded_syntastic_sh_sh_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! s:GetShell()
    if !exists('b:shell') || b:shell == ''
        let b:shell = ''
        let shebang = getbufline(bufnr('%'), 1)[0]
        if shebang != ''
            if stridx(shebang, 'bash') >= 0
                let b:shell = 'bash'
            elseif stridx(shebang, 'zsh') >= 0
                let b:shell = 'zsh'
            elseif stridx(shebang, 'sh') >= 0
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
    if registry.isCheckable('zsh')
        return registry.getCheckers('zsh', ['zsh'])[0].getLocListRaw()
    else
        return []
    endif
endfunction

function! s:IsShellValid()
    return len(s:GetShell()) > 0 && executable(s:GetShell())
endfunction


function! SyntaxCheckers_sh_sh_IsAvailable() dict
    return s:IsShellValid()
endfunction

function! SyntaxCheckers_sh_sh_GetLocList() dict
    if s:GetShell() ==# 'zsh'
        return s:ForwardToZshChecker()
    endif

    if !s:IsShellValid()
        return []
    endif

    let makeprg = self.makeprgBuild({
        \ 'exe': s:GetShell(),
        \ 'args_after': '-n' })

    let errorformat = '%f: line %l: %m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'sh',
    \ 'name': 'sh' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
