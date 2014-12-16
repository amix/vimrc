"============================================================================
"File:        mdl.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Charles Beynon <etothepiipower at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_markdown_mdl_checker")
    finish
endif
let g:loaded_syntastic_markdown_mdl_checker = 1

if !exists('g:syntastic_markdown_mdl_sort')
    let g:syntastic_markdown_mdl_sort = 1
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_markdown_mdl_GetLocList() dict
    let makeprg = self.makeprgBuild({ 'args': '--warnings' })

    let errorformat =
        \ '%E%f:%l: %m,'.
        \ '%W%f: Kramdown Warning: %m found on line %l'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'subtype': 'Style' })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'markdown',
    \ 'name': 'mdl'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
