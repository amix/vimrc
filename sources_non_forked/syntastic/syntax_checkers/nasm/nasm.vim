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

if exists('g:loaded_syntastic_nasm_nasm_checker')
    finish
endif
let g:loaded_syntastic_nasm_nasm_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_nasm_nasm_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'args_after': '-X gnu -f elf' .
        \       ' -I ' . syntastic#util#shescape(expand('%:p:h', 1) . syntastic#util#Slash()) .
        \       ' ' . syntastic#c#NullOutput() })

    let errorformat = '%f:%l: %t%*[^:]: %m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'nasm',
    \ 'name': 'nasm'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
