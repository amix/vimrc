"============================================================================
"File:        coffeelint.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Lincoln Stoll <l@lds.li>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_coffee_coffeelint_checker")
    finish
endif
let g:loaded_syntastic_coffee_coffeelint_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_coffee_coffeelint_GetLocList() dict
    if !exists('s:coffeelint_new')
        let ver = syntastic#util#getVersion(self.getExecEscaped() . ' --version')
        call self.log(self.getExec() . ' version =', ver)
        let s:coffeelint_new = syntastic#util#versionIsAtLeast(ver, [1, 4])
    endif
    let makeprg = self.makeprgBuild({ 'args_after': (s:coffeelint_new ? '--reporter csv' : '--csv') })

    let errorformat =
        \ '%f\,%l\,%\d%#\,%trror\,%m,' .
        \ '%f\,%l\,%trror\,%m,' .
        \ '%f\,%l\,%\d%#\,%tarn\,%m,' .
        \ '%f\,%l\,%tarn\,%m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'subtype': 'Style',
        \ 'returns': [0, 1] })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'coffee',
    \ 'name': 'coffeelint'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
