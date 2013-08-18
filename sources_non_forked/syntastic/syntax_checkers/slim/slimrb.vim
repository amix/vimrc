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
let g:loaded_syntastic_slim_slimrb_checker=1

function! SyntaxCheckers_slim_slimrb_IsAvailable()
    return executable("slimrb")
endfunction

function! s:SlimrbVersion()
    if !exists('s:slimrb_version')
        let s:slimrb_version = syntastic#util#parseVersion('slimrb --version 2>' . syntastic#util#DevNull())
    end
    return s:slimrb_version
endfunction

function! SyntaxCheckers_slim_slimrb_GetLocList()
    let makeprg = syntastic#makeprg#build({
        \ 'exe': 'slimrb',
        \ 'args': '-c',
        \ 'filetype': 'slim',
        \ 'subchecker': 'slimrb' })

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
