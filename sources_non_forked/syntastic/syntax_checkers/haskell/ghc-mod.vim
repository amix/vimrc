"============================================================================
"File:        ghc-mod.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Anthony Carapetis <anthony.carapetis at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_haskell_ghc_mod_checker")
    finish
endif
let g:loaded_syntastic_haskell_ghc_mod_checker=1

function! SyntaxCheckers_haskell_ghc_mod_IsAvailable()
    return executable('ghc-mod')
endfunction

function! SyntaxCheckers_haskell_ghc_mod_GetLocList()
    let errorformat =
        \ '%-G%\s%#,' .
        \ '%f:%l:%c:%trror: %m,' .
        \ '%f:%l:%c:%tarning: %m,'.
        \ '%f:%l:%c: %trror: %m,' .
        \ '%f:%l:%c: %tarning: %m,' .
        \ '%f:%l:%c:%m,' .
        \ '%E%f:%l:%c:,' .
        \ '%Z%m'

    let makeprg = syntastic#makeprg#build({
        \ 'exe': 'ghc-mod check',
        \ 'filetype': 'haskell',
        \ 'subchecker': 'ghc_mod' })

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'haskell',
    \ 'name': 'ghc_mod'})
