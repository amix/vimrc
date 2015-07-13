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

if exists('g:loaded_syntastic_sh_sh_checker')
    finish
endif
let g:loaded_syntastic_sh_sh_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_sh_sh_IsAvailable() dict " {{{1
    call self.log('shell =', s:GetShell())
    return s:IsShellValid()
endfunction " }}}1

function! SyntaxCheckers_sh_sh_GetLocList() dict " {{{1
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
endfunction " }}}1

" Utilities {{{1

function! s:GetShell() " {{{2
    if !exists('b:shell') || b:shell ==# ''
        let b:shell = ''
        let shebang = syntastic#util#parseShebang()['exe']
        if shebang !=# ''
            if shebang[-strlen('bash'):-1] ==# 'bash'
                let b:shell = 'bash'
            elseif shebang[-strlen('zsh'):-1] ==# 'zsh'
                let b:shell = 'zsh'
            elseif shebang[-strlen('sh'):-1] ==# 'sh'
                let b:shell = 'sh'
            endif
        endif
        " try to use env variable in case no shebang could be found
        if b:shell ==# ''
            let b:shell = fnamemodify($SHELL, ':t')
        endif
    endif
    return b:shell
endfunction " }}}2

function! s:IsShellValid() " {{{2
    let shell = s:GetShell()
    return shell !=# '' && executable(shell)
endfunction " }}}2

function! s:ForwardToZshChecker() " {{{2
    let registry = g:SyntasticRegistry.Instance()
    let zsh_checkers = registry.getCheckersAvailable('zsh', ['zsh'])
    if !empty(zsh_checkers)
        return zsh_checkers[0].getLocListRaw()
    else
        return []
    endif
endfunction " }}}2

" }}}1

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'sh',
    \ 'name': 'sh' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
