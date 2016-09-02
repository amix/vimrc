"============================================================================
"File:        rapper.vim
"Description: Syntax checking plugin for syntastic
"Maintainer:  Sebastian Tramp <mail@sebastian.tramp.name>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_turtle_rapper_checker')
    finish
endif
let g:loaded_syntastic_turtle_rapper_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_turtle_rapper_GetHighlightRegex(item)
    let term = matchstr(a:item['text'], '\mFailed to convert qname \zs\S\+\ze to URI')
    return term !=# '' ? '\V\<' . escape(term, '\') . '\>' : ''
endfunction

function! SyntaxCheckers_turtle_rapper_GetLocList() dict
    let makeprg = self.makeprgBuild({ 'args': '-i guess -q --count' })

    let errorformat =
        \ 'rapper: %trror - URI file://%f:%l - %m,' .
        \ 'rapper: %tarning - URI file://%f:%l - %m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'returns': [0, 1] })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'turtle',
    \ 'name': 'rapper'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
