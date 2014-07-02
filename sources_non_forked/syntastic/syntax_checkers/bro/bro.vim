"============================================================================
"File:        bro.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Justin Azoff <justin.azoff@gmail.com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_bro_bro_checker")
    finish
endif
let g:loaded_syntastic_bro_bro_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_bro_bro_IsAvailable() dict
    return system(self.getExecEscaped() . ' --help') =~# '--parse-only'
endfunction

function! SyntaxCheckers_bro_bro_GetLocList() dict
    let makeprg = self.makeprgBuild({ 'args_before': '--parse-only' })

    "example: error in ./foo.bro, line 3: unknown identifier banana, at or "near "banana"
    let errorformat =
        \ '%trror in %f\, line %l: %m,' .
        \ '%tarning in %f\, line %l: %m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'bro',
    \ 'name': 'bro'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
