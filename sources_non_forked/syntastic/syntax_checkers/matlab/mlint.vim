"============================================================================
"File:        matlab.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Jason Graham <jason at the-graham dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_matlab_mlint_checker")
    finish
endif
let g:loaded_syntastic_matlab_mlint_checker=1

function! SyntaxCheckers_matlab_mlint_IsAvailable()
    return executable("mlint")
endfunction

function! SyntaxCheckers_matlab_mlint_GetLocList()
    let makeprg = syntastic#makeprg#build({
        \ 'exe': 'mlint',
        \ 'args': '-id $*',
        \ 'filetype': 'matlab',
        \ 'subchecker': 'mlint' })

    let errorformat =
        \ 'L %l (C %c): %*[a-zA-Z0-9]: %m,'.
        \ 'L %l (C %c-%*[0-9]): %*[a-zA-Z0-9]: %m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'defaults': {'bufnr': bufnr("")} })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'matlab',
    \ 'name': 'mlint'})
