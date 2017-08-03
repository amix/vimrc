"============================================================================
"File:        phpstan.vim
"Description: Syntax checking plugin for syntastic
"Maintainer:  Przepompownia przepompownia@users.noreply.github.com
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_php_phpstan_checker')
    finish
endif
let g:loaded_syntastic_php_phpstan_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_php_phpstan_IsAvailable() dict
    if !executable(self.getExec())
        return 0
    endif
    return syntastic#util#versionIsAtLeast(self.getVersion(), [0, 7])
endfunction

function! SyntaxCheckers_php_phpstan_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'exe_after': 'analyse',
        \ 'args': '--level=5',
        \ 'args_after': '--errorFormat raw' })

    let errorformat = '%f:%l:%m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'subtype' : 'Style' })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'php',
    \ 'name': 'phpstan'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
