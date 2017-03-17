"============================================================================
"File:        nix.vim
"Description: Check nix syntax using 'nix-instantiate --eval-only'
"Maintainer:  Tim Cuthbertson <tim@gfxmonk.net>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================
"
"
if exists('g:loaded_syntastic_nix_nix_checker')
    finish
endif
let g:loaded_syntastic_nix_nix_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_nix_nix_GetLocList() dict
    let makeprg = self.makeprgBuild({ 'args_after': '--parse-only' })

    let errorformat =
        \ '%f:%l:%c:%m,' .
        \ '%f:%l:%m,' .
        \ '%f:%m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'defaults': {'type': 'e'},
        \ 'preprocess': 'nix' })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'nix',
    \ 'name': 'nix',
    \ 'exec': 'nix-instantiate' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
