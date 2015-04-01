"============================================================================
"File:        handlebars.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Martin Grenfell <martin.grenfell at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"============================================================================

if exists("g:loaded_syntastic_handlebars_handlebars_checker")
    finish
endif
let g:loaded_syntastic_handlebars_handlebars_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_handlebars_handlebars_GetLocList() dict
    let makeprg = self.makeprgBuild({ 'args_after': '-f ' . syntastic#util#DevNull() })

    let errorformat =
        \ '%EError: %m on line %l:,'.
        \ "%EError: %m,".
        \ '%Z%p^,' .
        \ '%-G%.%#'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'postprocess': ['guards'],
        \ 'defaults': {'bufnr': bufnr("")} })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'handlebars',
    \ 'name': 'handlebars'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
