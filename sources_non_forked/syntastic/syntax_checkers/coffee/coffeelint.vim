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
let g:loaded_syntastic_coffee_coffeelint_checker=1

function! SyntaxCheckers_coffee_coffeelint_IsAvailable()
    return executable('coffeelint')
endfunction

function! SyntaxCheckers_coffee_coffeelint_GetLocList()
    let makeprg = syntastic#makeprg#build({
        \ 'exe': 'coffeelint',
        \ 'args': '--csv',
        \ 'filetype': 'coffee',
        \ 'subchecker': 'coffeelint' })

    let errorformat = '%f\,%l\,%trror\,%m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'subtype': 'Style' })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'coffee',
    \ 'name': 'coffeelint'})
