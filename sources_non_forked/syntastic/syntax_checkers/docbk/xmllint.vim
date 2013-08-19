"============================================================================
"File:        docbk.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Martin Grenfell <martin.grenfell at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_docbk_xmllint_checker")
    finish
endif
let g:loaded_syntastic_docbk_xmllint_checker=1

function! SyntaxCheckers_docbk_xmllint_IsAvailable()
    return SyntaxCheckers_xml_xmllint_IsAvailable()
endfunction

function! SyntaxCheckers_docbk_xmllint_GetLocList()
    return SyntaxCheckers_xml_xmllint_GetLocList()
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'docbk',
    \ 'name': 'xmllint'})

runtime! syntax_checkers/xml/*.vim
