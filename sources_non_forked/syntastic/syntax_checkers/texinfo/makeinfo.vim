"============================================================================
"File:        makeinfo.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_texinfo_makeinfo_checker")
    finish
endif
let g:loaded_syntastic_texinfo_makeinfo_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_texinfo_makeinfo_GetHighlightRegex(item)
    let term = matchstr(a:item['text'], "\\m`\\zs[^']\\+\\ze'")
    return term != '' ? '\V' . term : ''
endfunction

function! SyntaxCheckers_texinfo_makeinfo_GetLocList() dict
    let makeprg = self.makeprgBuild({ 'args_after': syntastic#c#NullOutput() })

    let errorformat =
        \ '%f:%l: %tarning: %m,' .
        \ '%f:%l: %m,' .
        \ '%-G%.%#'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'type': 'e' })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'texinfo',
    \ 'name': 'makeinfo'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
