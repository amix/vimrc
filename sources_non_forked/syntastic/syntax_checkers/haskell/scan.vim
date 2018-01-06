"============================================================================
"File:        scan.vim
"Description: Syntax checking plugin for syntastic
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_haskell_scan_checker')
    finish
endif
let g:loaded_syntastic_haskell_scan_checker = 1

if !exists('g:syntastic_haskell_scan_sort')
    let g:syntastic_haskell_scan_sort = 1
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_haskell_scan_GetLocList() dict
    let makeprg = self.makeprgBuild({})

    let errorformat = '%f:%l:%v: %m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'subtype': 'Style' })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'haskell',
    \ 'name': 'scan'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
