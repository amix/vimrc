"============================================================================
"File:        slim.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Martin Grenfell <martin.grenfell at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_slim_slimrb_checker")
    finish
endif
let g:loaded_syntastic_slim_slimrb_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! s:SlimrbVersion()
    if !exists('s:slimrb_version')
        let s:slimrb_version = syntastic#util#getVersion('slimrb --version 2>' . syntastic#util#DevNull())
    endif
    return s:slimrb_version
endfunction

function! SyntaxCheckers_slim_slimrb_GetLocList() dict
    let makeprg = self.makeprgBuild({ 'args_after': '-c' })

    if syntastic#util#versionIsAtLeast(s:SlimrbVersion(), [1,3,1])
        let errorformat =
            \ '%C\ %#%f\, Line %l\, Column %c,'.
            \ '%-G\ %.%#,'.
            \ '%ESlim::Parser::SyntaxError: %m,'.
            \ '%+C%.%#'
    else
        let errorformat =
            \ '%C\ %#%f\, Line %l,'.
            \ '%-G\ %.%#,'.
            \ '%ESlim::Parser::SyntaxError: %m,'.
            \ '%+C%.%#'
    endif

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'slim',
    \ 'name': 'slimrb'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
