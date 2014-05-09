"============================================================================
"File:        rust.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Chad Jablonski <chad.jablonski at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_rust_rustc_checker")
    finish
endif
let g:loaded_syntastic_rust_rustc_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_rust_rustc_GetLocList() dict
    let makeprg = self.makeprgBuild({ 'args_after': '--no-trans' })

    let errorformat  =
        \ '%E%f:%l:%c: %\d%#:%\d%# %.%\{-}error:%.%\{-} %m,'   .
        \ '%W%f:%l:%c: %\d%#:%\d%# %.%\{-}warning:%.%\{-} %m,' .
        \ '%C%f:%l %m,' .
        \ '%-Z%.%#'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'rust',
    \ 'name': 'rustc'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
