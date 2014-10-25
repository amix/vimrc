"============================================================================
"File:        chktex.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_tex_chktex_checker')
    finish
endif
let g:loaded_syntastic_tex_chktex_checker = 1

let s:save_cpo = &cpo
set cpo&vim

if !exists('g:syntastic_tex_chktex_showmsgs')
    let g:syntastic_tex_chktex_showmsgs = 1
endif

function! SyntaxCheckers_tex_chktex_GetLocList() dict
    let makeprg = self.makeprgBuild({ 'args_after': '-q -v1' })

    let errorformat =
        \ '%EError %n in %f line %l: %m,' .
        \ '%WWarning %n in %f line %l: %m,' .
        \ (g:syntastic_tex_chktex_showmsgs ? '%WMessage %n in %f line %l: %m,' : '') .
        \ '%Z%p^,' .
        \ '%-G%.%#'

    let loclist =  SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'subtype': 'Style' })

    call self.setWantSort(1)

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'tex',
    \ 'name': 'chktex'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
