"============================================================================
"File:        co.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Andrew Kelley <superjoe30@gmail.com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_co_coco_checker")
    finish
endif
let g:loaded_syntastic_co_coco_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_co_coco_GetLocList() dict
    let tmpdir = syntastic#util#tmpdir()
    let makeprg = self.makeprgBuild({ 'args_after': '-c -o ' . tmpdir })

    let errorformat =
        \ '%EFailed at: %f,' .
        \ '%ZSyntax%trror: %m on line %l,'.
        \ '%EFailed at: %f,'.
        \ '%Z%trror: Parse error on line %l: %m'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })

    call syntastic#util#rmrf(tmpdir)

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'co',
    \ 'name': 'coco'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
