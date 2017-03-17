"============================================================================
"File:        msgfmt.vim
"Description: Syntax checking plugin for po files of gettext
"Maintainer:  Ryo Okubo <syucream1031@gmail.com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_po_msgfmt_checker')
    finish
endif
let g:loaded_syntastic_po_msgfmt_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_po_msgfmt_GetHighlightRegex(item)
    let term = matchstr(a:item['text'], '\mkeyword "\zs[^"]\+\ze" unknown')
    return term !=# '' ? '\V' . escape(term, '\') : ''
endfunction

function! SyntaxCheckers_po_msgfmt_GetLocList() dict
    let makeprg = self.makeprgBuild({ 'args_after': '-c ' . syntastic#c#NullOutput() })

    let errorformat =
        \ '%W%f:%l: warning: %m,' .
        \ '%E%f:%l:%v: %m,' .
        \ '%E%f:%l: %m,' .
        \ '%+C %.%#,' .
        \ '%Z%p^,' .
        \ '%-G%.%#'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'postprocess': ['compressWhitespace'] })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'po',
    \ 'name': 'msgfmt'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
