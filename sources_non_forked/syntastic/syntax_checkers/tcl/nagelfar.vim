"============================================================================
"File:        nagelfar.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  James Pickard <james.pickard at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================
"
"Notes:       Requires nagelfar v1.1.12 or later with support for -H option.
"             See nagelfar homepage http://nagelfar.berlios.de/.

if exists("g:loaded_syntastic_tcl_nagelfar_checker")
    finish
endif
let g:loaded_syntastic_tcl_nagelfar_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_tcl_nagelfar_GetLocList() dict
    let makeprg = self.makeprgBuild({ 'args_after': '-H' })

    let errorformat =
        \ '%I%f: %l: N %m,'.
        \ '%f: %l: %t %m,'.
        \ '%-GChecking file %f'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'tcl',
    \ 'name': 'nagelfar'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
