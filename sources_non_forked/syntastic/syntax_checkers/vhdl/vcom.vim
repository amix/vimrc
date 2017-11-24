"============================================================================
"File:        vcom.vim
"Description: Syntax checking plugin for syntastic
"Maintainer:  Jim Vogel <jim dot e dot vogel at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_vhdl_vcom_checker')
    finish
endif
let g:loaded_syntastic_vhdl_vcom_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_vhdl_vcom_GetLocList() dict
    let makeprg = self.makeprgBuild({ 'args_before': '-lint' })

    let errorformat =
        \ '** %tRROR: %f(%l): %m,' .
        \ '** %tRROR: %m,' .
        \ '** %tARNING: %f(%l): %m,' .
        \ '** %tARNING: %m,' .
        \ '** %tOTE: %m,' .
        \ '%tRROR: %f(%l): %m,' .
        \ '%tARNING[%*[0-9]]: %f(%l): %m,' .
        \ '%tRROR: %m,' .
        \ '%tARNING[%*[0-9]]: %m'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })

    for e in loclist
        if e['type'] !=? 'E' && e['type'] !=? 'W'
            let e['type'] = 'W'
        endif
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'vhdl',
    \ 'name': 'vcom'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
