"============================================================================
"File:        mixedindentlint.vim
"Description: Mixed indentation linter for vim
"Maintainer:  Payton Swick <payton@foolord.com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"============================================================================

if exists('g:loaded_syntastic_css_mixedindentlint_checker')
    finish
endif
let g:loaded_syntastic_css_mixedindentlint_checker = 1

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'css',
    \ 'name': 'mixedindentlint',
    \ 'redirect': 'javascript/mixedindentlint'})

" vim: set et sts=4 sw=4:
