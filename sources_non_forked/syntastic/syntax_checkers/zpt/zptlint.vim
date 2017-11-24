"============================================================================
"File:        zpt.vim
"Description: Syntax checking plugin for syntastic
"Maintainer:  claytron <robots at claytron dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_zpt_zptlint_checker')
    finish
endif
let g:loaded_syntastic_zpt_zptlint_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_zpt_zptlint_GetLocList() dict
    let makeprg = self.makeprgBuild({})

    let errorformat=
        \ '%-P*** Error in: %f,'.
        \ '%Z%*\s\, at line %l\, column %c,'.
        \ '%E%*\s%m,'.
        \ '%-Q'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'zpt',
    \ 'name': 'zptlint'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
