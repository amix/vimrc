"============================================================================
"File:        cppcheck.vim
"Description: Syntax checking plugin for syntastic.vim using cppcheck.pl
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"============================================================================
"
" The setting 'g:syntastic_cppcheck_config_file' allows you to define a file
" that contains additional compiler arguments like include directories or
" CFLAGS. The file is expected to contain one option per line. If none is
" given the filename defaults to '.syntastic_cppcheck_config':
"
"   let g:syntastic_cppcheck_config_file = '.config'

if exists("g:loaded_syntastic_cpp_cppcheck_checker")
    finish
endif
let g:loaded_syntastic_cpp_cppcheck_checker = 1

runtime! syntax_checkers/c/*.vim

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'cpp',
    \ 'name': 'cppcheck',
    \ 'redirect': 'c/cppcheck'})

" vim: set et sts=4 sw=4:
