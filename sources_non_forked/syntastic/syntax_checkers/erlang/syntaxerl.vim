"============================================================================
"File:        syntaxerl.vim
"Description: Syntax checking plugin for syntastic.
"Maintainer:  locojay
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"============================================================================

if exists("g:loaded_syntastic_erlang_syntaxerl_checker")
    finish
endif

let g:loaded_syntastic_erlang_syntaxerl_checker = 1

let s:save_cpo = &cpo
set cpo&vim


function! SyntaxCheckers_erlang_syntaxerl_GetLocList() dict

    let makeprg = self.makeprgBuild({})

    let errorformat =
        \ '%W%f:%l: warning: %m,'.
        \ '%E%f:%l: %m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'erlang',
    \ 'name': 'syntaxerl'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
