"============================================================================
"File:        avrgcc.vim
"Description: Syntax checking plugin for syntastic
"Maintainer:  Sławek Piotrowski <sentinel at atteo dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_cpp_avrgcc_checker')
    finish
endif
let g:loaded_syntastic_cpp_avrgcc_checker = 1

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'cpp',
    \ 'name': 'avrgcc',
    \ 'exec': 'avr-g++',
    \ 'redirect': 'c/avrgcc'})

" vim: set sw=4 sts=4 et fdm=marker:
