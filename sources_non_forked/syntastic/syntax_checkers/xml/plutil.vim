"============================================================================
"File:        plutil.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_xml_plutil_checker")
    finish
endif
let g:loaded_syntastic_xml_plutil_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_xml_plutil_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'args_before': '-lint -s',
        \ 'fname_before': '--' })

    let errorformat =
        \ '%E%f: %m at line %l'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'returns': [0, 1] })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'xml',
    \ 'name': 'plutil'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
