"============================================================================
"File:        oclint.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  "UnCO" Lin <undercooled aT lavabit com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"============================================================================
"
" The setting 'g:syntastic_oclint_config_file' allows you to define a file
" that contains additional compiler arguments like include directories or
" CFLAGS. The file is expected to contain one option per line. If none is
" given the filename defaults to '.syntastic_oclint_config':
"
"   let g:syntastic_oclint_config_file = '.config'

if exists("g:loaded_syntastic_cpp_oclint_checker")
    finish
endif
let g:loaded_syntastic_cpp_oclint_checker = 1

function! SyntaxCheckers_cpp_oclint_IsAvailable()
    return SyntaxCheckers_c_oclint_IsAvailable()
endfunction

function! SyntaxCheckers_cpp_oclint_GetLocList()
    return SyntaxCheckers_c_oclint_GetLocList()
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'cpp',
    \ 'name': 'oclint'})

runtime! syntax_checkers/c/*.vim
