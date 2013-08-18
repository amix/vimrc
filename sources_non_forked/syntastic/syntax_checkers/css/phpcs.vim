"============================================================================
"File:        phpcs.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================
"
" See here for details of phpcs
"    - phpcs (see http://pear.php.net/package/PHP_CodeSniffer)
"
if exists("g:loaded_syntastic_css_phpcs_checker")
    finish
endif
let g:loaded_syntastic_css_phpcs_checker=1

function! SyntaxCheckers_css_phpcs_IsAvailable()
    return SyntaxCheckers_php_phpcs_IsAvailable()
endfunction

function! SyntaxCheckers_css_phpcs_GetLocList()
    return SyntaxCheckers_php_phpcs_GetLocList()
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'css',
    \ 'name': 'phpcs'})

runtime! syntax_checkers/php/*.vim
