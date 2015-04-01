"============================================================================
"File:        rnv.vim
"Description: RelaxNG RNV syntax checking plugin for syntastic.vim
"Maintainer:  Remko Tronçon <remko at el-tramo dot be>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"============================================================================

if exists("g:loaded_syntastic_rnc_rnv_checker")
    finish
endif
let g:loaded_syntastic_rnc_rnv_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_rnc_rnv_GetLocList() dict
    let makeprg = self.makeprgBuild({ 'args': '-c' })

    let errorformat =
        \ '%f:%l:%c: %trror: %m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'rnc',
    \ 'name': 'rnv'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
