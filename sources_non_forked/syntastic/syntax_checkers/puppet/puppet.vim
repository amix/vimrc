"============================================================================
"File:        puppet.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Eivind Uggedal <eivind at uggedal dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_puppet_puppet_checker')
    finish
endif
let g:loaded_syntastic_puppet_puppet_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_puppet_puppet_GetLocList() dict
    if !exists('s:puppet_new')
        let s:puppet_new = syntastic#util#versionIsAtLeast(self.getVersion(), [2, 7, 0])
    endif

    let makeprg = self.makeprgBuild({
        \ 'args_before': (s:puppet_new ? 'parser validate --color=false' : '--color=false --parseonly') })

    let errorformat =
        \ '%-Gerr: Try ''puppet help parser validate'' for usage,' .
        \ '%-GError: Try ''puppet help parser validate'' for usage,' .
        \ '%A%t%*[a-zA-Z]: %m at %f:%l:%c,' .
        \ '%A%t%*[a-zA-Z]: %m at %f:%l'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'puppet',
    \ 'name': 'puppet'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
