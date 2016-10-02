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

if exists('g:loaded_syntastic_haskell_hdevtools_checker')
    finish
endif
let g:loaded_syntastic_haskell_hdevtools_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_haskell_hdevtools_GetLocList() dict
    if !exists('g:syntastic_haskell_hdevtools_args') && exists('g:hdevtools_options')
        call syntastic#log#oneTimeWarn('variable g:hdevtools_options is deprecated, ' .
            \ 'please use g:syntastic_haskell_hdevtools_args instead')
        let g:syntastic_haskell_hdevtools_args = g:hdevtools_options
    endif

    let makeprg = self.makeprgBuild({
        \ 'exe_after': 'check',
        \ 'fname': syntastic#util#shexpand('%:p') })

    let errorformat =
        \ '%-Z %#,'.
        \ '%W%\m%f:%l:%v%\%%(-%\d%\+%\)%\=: Warning: %m,'.
        \ '%W%\m%f:%l:%v%\%%(-%\d%\+%\)%\=: Warning:,'.
        \ '%E%\m%f:%l:%v%\%%(-%\d%\+%\)%\=: %m,'.
        \ '%E%>%\m%f:%l:%v%\%%(-%\d%\+%\)%\=:,'.
        \ '%+C  %#%m,'.
        \ '%W%>%\m%f:%l:%v%\%%(-%\d%\+%\)%\=:,'.
        \ '%+C  %#%tarning: %m,'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'defaults': {'vcol': 1},
        \ 'postprocess': ['compressWhitespace'] })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'haskell',
    \ 'name': 'hdevtools'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
