"============================================================================
"File:        mandoc.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_nroff_mandoc_checker")
    finish
endif
let g:loaded_syntastic_nroff_mandoc_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_nroff_mandoc_GetLocList() dict
    let makeprg = self.makeprgBuild({ 'args_after': '-Tlint' })

    let errorformat =
        \ '%E%f:%l:%c: %tRROR: %m,' .
        \ '%W%f:%l:%c: %tARNING: %m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'returns': [0, 2, 3, 4] })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'nroff',
    \ 'name': 'mandoc'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
