"============================================================================
"File:        recess.vim
"Description: Syntax checking plugin for syntastic.vim using `recess`
"             (http://twitter.github.io/recess/).
"Maintainer:  Tim Carry <tim at pixelastic dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_css_recess_checker")
    finish
endif
let g:loaded_syntastic_css_recess_checker = 1

runtime! syntax_checkers/less/*.vim

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'css',
    \ 'name': 'recess',
    \ 'redirect': 'less/recess'})

" vim: set et sts=4 sw=4:
