"============================================================================
"File:        dscanner.vim
"Description: Syntax checking plugin for syntastic
"Maintainer:  ANtlord
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_d_dscanner_checker')
    finish
endif
let g:loaded_syntastic_d_dscanner_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_d_dscanner_GetHighlightRegex(i)
    let term = matchstr(a:i['text'], '\m^.\{-}''\zs\S\+\ze''')
    return term !=# '' ? '\V\<' . escape(term, '\') . '\>' : ''
endfunction

function! SyntaxCheckers_d_dscanner_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'args_after': '--report',
        \ 'tail': '2>' . syntastic#util#DevNull() })

    let errorformat = '%f:%l:%c:%m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'preprocess': 'dscanner',
        \ 'subtype': 'Style',
        \ 'returns': [0] })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'd',
    \ 'name': 'dscanner' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
