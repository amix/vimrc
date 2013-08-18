"============================================================================
"File:        podchecker.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_perl_podchecker_checker")
    finish
endif
let g:loaded_syntastic_perl_podchecker_checker=1

function! SyntaxCheckers_perl_podchecker_IsAvailable()
    return SyntaxCheckers_pod_podchecker_IsAvailable()
endfunction

function! SyntaxCheckers_perl_podchecker_GetLocList()
    return SyntaxCheckers_pod_podchecker_GetLocList()
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'perl',
    \ 'name': 'podchecker'})

runtime! syntax_checkers/pod/*.vim
