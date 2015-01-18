"============================================================================
"File:        xml.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Sebastian Kusnier <sebastian at kusnier dot net>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_xml_xmllint_checker")
    finish
endif
let g:loaded_syntastic_xml_xmllint_checker = 1

let s:save_cpo = &cpo
set cpo&vim

" You can use a local installation of DTDs to significantly speed up validation
" and allow you to validate XML data without network access, see xmlcatalog(1)
" and http://www.xmlsoft.org/catalog.html for more information.

function! SyntaxCheckers_xml_xmllint_GetLocList() dict
    let makeprg = self.makeprgBuild({ 'args_after': '--xinclude --noout --postvalid' })

    let errorformat=
        \ '%E%f:%l: error : %m,' .
        \ '%-G%f:%l: validity error : Validation failed: no DTD found %m,' .
        \ '%W%f:%l: warning : %m,' .
        \ '%W%f:%l: validity warning : %m,' .
        \ '%E%f:%l: validity error : %m,' .
        \ '%E%f:%l: parser error : %m,' .
        \ '%E%f:%l: %m,' .
        \ '%-Z%p^,' .
        \ '%-G%.%#'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'returns': [0, 1, 2, 3, 4, 5] })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'xml',
    \ 'name': 'xmllint'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
