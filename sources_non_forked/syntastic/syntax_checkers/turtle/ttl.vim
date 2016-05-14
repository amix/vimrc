"============================================================================
"File:        ttl.vim
"Description: turtle syntax checker - using ttl from turtle-validator (npm)
"Maintainer:  Antoine Reilles (tonio@NetBSD.org)
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"============================================================================

if exists('g:loaded_syntastic_turtle_ttl_checker')
    finish
endif
let g:loaded_syntastic_turtle_ttl_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_turtle_ttl_GetHighlightRegex(item)
    let term = matchstr(a:item['text'], '\m"\zs[^"]\+\ze"')
    return term !=# '' ? '\V\<' . escape(term, '\') . '\>' : ''
endfunction

function! SyntaxCheckers_turtle_ttl_GetLocList() dict
    let makeprg = self.makeprgBuild({})

    let errorformat = '%\m[Error: %m %\%%(at%\|on%\) line %l%\%.]'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'defaults': {'bufnr': bufnr('')} })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'turtle',
    \ 'name': 'ttl'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
