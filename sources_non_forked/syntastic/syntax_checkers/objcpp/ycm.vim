"============================================================================
"File:        ycm.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Val Markovic <val at markovic dot io>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_objcpp_ycm_checker")
    finish
endif
let g:loaded_syntastic_objcpp_ycm_checker = 1

runtime! syntax_checkers/c/*.vim

function! SyntaxCheckers_objcpp_ycm_IsAvailable()
    return SyntaxCheckers_c_ycm_IsAvailable()
endfunction

if !exists('g:loaded_youcompleteme')
    finish
endif

function! SyntaxCheckers_objcpp_ycm_GetLocList()
    return SyntaxCheckers_c_ycm_GetLocList()
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'objcpp',
    \ 'name': 'ycm'})
