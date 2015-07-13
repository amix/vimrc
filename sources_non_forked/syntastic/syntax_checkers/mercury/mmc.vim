"============================================================================
"File:        mercury.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Joshua Rahm (joshuarahm@gmail.com)
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_mercury_mmc_checker')
    finish
endif
let g:loaded_syntastic_mercury_mmc_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_mercury_mmc_GetLocList() dict
    let makeprg = self.makeprgBuild({ 'args_before': '-e' })

    let errorformat =
        \ '%C%f:%l:  %m,' .
        \ '%E%f:%l: %m,' .
        \ '%-G%.%#'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })

    for e in loclist
        if stridx(e['text'], ' warning:') >= 0
            let e['type'] = 'W'
        endif
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'mercury',
    \ 'name': 'mmc'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
