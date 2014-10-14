"============================================================================
"File:        igor.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_docbk_igor_checker')
    finish
endif
let g:loaded_syntastic_docbk_igor_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_docbk_igor_GetLocList() dict
    let makeprg = self.makeprgBuild({})

    let errorformat = '%f:%l:%m'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'defaults': { 'type': 'W' },
        \ 'subtype': 'Style',
        \ 'returns': [0] })

    let buf = bufnr('')
    for e in loclist
        " XXX: igor strips directories from filenames
        let e['bufnr'] = buf

        let e['hl'] = '\V' . escape( substitute(e['text'], '\m[^:]*:', '', ''), '\' )
        let e['hl'] = substitute(e['hl'], '\V[', '\\zs', 'g')
        let e['hl'] = substitute(e['hl'], '\V]', '\\ze', 'g')

        " let e['text'] = substitute(e['text'], '\m:.*$', '', '')
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'docbk',
    \ 'name': 'igor'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
