"============================================================================
"File:        smlnj.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_sml_smlnj_checker')
    finish
endif
let g:loaded_syntastic_sml_smlnj_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_sml_smlnj_GetLocList() dict
    let makeprg = self.makeprgBuild({})

    let errorformat =
        \ '%E%f:%l%\%.%c %trror: %m,' .
        \ '%E%f:%l%\%.%c-%\d%\+%\%.%\d%\+ %trror: %m,' .
        \ '%W%f:%l%\%.%c %tarning: %m,' .
        \ '%W%f:%l%\%.%c-%\d%\+%\%.%\d%\+ %tarning: %m,' .
        \ '%C%\s%\+%m,' .
        \ '%-G%.%#'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'postprocess': ['compressWhitespace'],
        \ 'returns': [0, 1] })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'sml',
    \ 'name': 'smlnj',
    \ 'exec': 'sml'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
