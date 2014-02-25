"============================================================================
"File:        nasm.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Håvard Pettersson <haavard.pettersson at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================
if exists("g:loaded_syntastic_nasm_nasm_checker")
    finish
endif
let g:loaded_syntastic_nasm_nasm_checker=1

function! SyntaxCheckers_nasm_nasm_IsAvailable()
    return executable("nasm")
endfunction

function! SyntaxCheckers_nasm_nasm_GetLocList()
    let wd = syntastic#util#shescape(expand("%:p:h") . "/")
    let makeprg = syntastic#makeprg#build({
        \ 'exe': 'nasm',
        \ 'args': '-X gnu -f elf -I ' . wd . ' ' . syntastic#c#GetNullDevice()
        \ 'filetype': 'nasm',
        \ 'subchecker': 'nasm' })

    let errorformat = '%f:%l: %t%*[^:]: %m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'nasm',
    \ 'name': 'nasm'})
