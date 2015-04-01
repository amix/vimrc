"============================================================================
"File:        jshint.vim
"Description: Javascript syntax checker for xHTML - using jshint
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_xhtml_jshint_checker")
    finish
endif
let g:loaded_syntastic_xhtml_jshint_checker = 1

runtime! syntax_checkers/html/*.vim

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'xhtml',
    \ 'name': 'jshint',
    \ 'redirect': 'html/jshint'})

" vim: set sw=4 sts=4 et fdm=marker:
