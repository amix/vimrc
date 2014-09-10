"============================================================================
"File:        avrgcc.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Karel <karelishere at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_arduino_avrgcc_checker')
    finish
endif
let g:loaded_syntastic_arduino_avrgcc_checker = 1

runtime! syntax_checkers/c/*.vim

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'c',
    \ 'name': 'avrgcc',
    \ 'exec': 'avr-gcc',
    \ 'redirect': 'c/avrgcc'})

" vim: set et sts=4 sw=4:
