"============================================================================
"File:        flawfinder.vim
"Description: Syntax checking plugin for syntastic
"Maintainer:  Benjamin Bannier <bbannier at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"============================================================================

if exists('g:loaded_syntastic_cpp_flawfinder_checker')
    finish
endif
let g:loaded_syntastic_cpp_flawfinder_checker = 1

if !exists('g:syntastic_cpp_flawfinder_thres')
    let g:syntastic_cpp_flawfinder_thres = 3
endif

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'cpp',
    \ 'name': 'flawfinder',
    \ 'redirect': 'c/flawfinder'})

" vim: set sw=4 sts=4 et fdm=marker:
