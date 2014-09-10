"============================================================================
"File:        hdevtools.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Anthony Carapetis <anthony.carapetis at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_haskell_hdevtools_checker")
    finish
endif
let g:loaded_syntastic_haskell_hdevtools_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_haskell_hdevtools_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'exe': self.getExecEscaped() . ' check',
        \ 'args': get(g:, 'hdevtools_options', '') })

    let errorformat= '\%-Z\ %#,'.
        \ '%W%f:%l:%c:\ Warning:\ %m,'.
        \ '%W%f:%l:%c:\ Warning:,'.
        \ '%E%f:%l:%c:\ %m,'.
        \ '%E%>%f:%l:%c:,'.
        \ '%+C\ \ %#%m,'.
        \ '%W%>%f:%l:%c:,'.
        \ '%+C\ \ %#%tarning:\ %m,'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'postprocess': ['compressWhitespace'] })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'haskell',
    \ 'name': 'hdevtools'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
