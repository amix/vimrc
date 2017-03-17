"============================================================================
"File:        stylint.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_stylus_stylint_checker')
    finish
endif
let g:loaded_syntastic_stylus_stylint_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_stylus_stylint_GetLocList() dict
    if !exists('s:stylint_new')
        let s:stylint_new = syntastic#util#versionIsAtLeast(self.getVersion(), [1, 5, 7])
    endif

    let makeprg = self.makeprgBuild({})

    if s:stylint_new
        let errorformat =
            \ '%P%f,' .
            \ '%-Q,' .
            \ '%\m%l:%c%\s%\+%t%\%%(rror%\|arning%\)%\s%\+%m%\s%\+%\S%\+%\s%#,' .
            \ '%\m%l%\s%\+%t%\%%(rror%\|arning%\)%\s%\+%m%\s%\+%\S%\+%\s%#'
    else
        let errorformat =
            \ '%WWarning: %m,' .
            \ '%EError: %m,' .
            \ '%CFile: %f,' .
            \ '%CLine: %l:%.%#'
    endif

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'subtype': 'Style' })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'stylus',
    \ 'name': 'stylint'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
