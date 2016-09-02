"============================================================================
"File:        textlint.vim
"Description: Syntax checking plugin for syntastic
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_text_textlint_checker')
    finish
endif
let g:loaded_syntastic_text_textlint_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_text_textlint_GetLocList() dict
    let makeprg = self.makeprgBuild({ 'args_after': '-f compact' })

    let errorformat =
        \ '%f: line %l\, col %c\, %tarning - %m,' .
        \ '%f: line %l\, col %c\, %trror - %m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'subtype': 'Style',
        \ 'returns': [0, 1] })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'text',
    \ 'name': 'textlint'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
