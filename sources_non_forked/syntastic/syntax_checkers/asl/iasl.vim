"============================================================================
"File:        iasl.vim
"Description: Syntax checking plugin for syntastic using iasl
"Maintainer:  Peter Wu <peter@lekensteyn.nl>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"============================================================================

if exists('g:loaded_syntastic_asl_iasl_checker')
    finish
endif
let g:loaded_syntastic_asl_iasl_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_asl_iasl_GetLocList() dict
    let tmpdir = syntastic#util#tmpdir() . syntastic#util#Slash()
    let makeprg = self.makeprgBuild({
        \ 'args': '-vi',
        \ 'args_after': ['-p', tmpdir] })

    let errorformat =
        \ '%f(%l) : %trror    %n - %m,' .
        \ '%f(%l) : %tarning  %n - %m,' .
        \ '%f(%l) : %temark   %n - %m,' .
        \ '%f(%l) : %tptimize %n - %m,' .
        \ '%f(%l) : %m'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'returns': [0, 255] })

    for e in loclist
        if e['type'] =~? 'r'
            let e['type'] = 'W'
        elseif e['type'] =~? 'o'
            let e['type'] = 'W'
            let e['subtype'] = 'Style'
        endif
    endfor

    call syntastic#util#rmrf(tmpdir)

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'asl',
    \ 'name': 'iasl'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
