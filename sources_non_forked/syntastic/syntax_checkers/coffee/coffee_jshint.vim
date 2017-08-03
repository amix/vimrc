"============================================================================
"File:        coffee_jshint.vim
"Description: Syntax checking plugin for syntastic
"Maintainer:  John Krauss <john@johnkrauss.com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_coffee_coffee_jshint_checker')
    finish
endif
let g:loaded_syntastic_coffee_coffee_jshint_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_coffee_coffee_jshint_GetLocList() dict
    let makeprg = self.makeprgBuild({})

    let errorformat =
        \ '%Q-%\{32\,},' .
        \ '%E%l:%c: %m,' .
        \ '%P%f'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'returns': [0, 1] })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'coffee',
    \ 'exec': 'coffee-jshint',
    \ 'name': 'coffee_jshint' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:

