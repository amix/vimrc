"============================================================================
"File:        oclint.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  "UnCO" Lin <undercooled aT lavabit com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"============================================================================

if exists('g:loaded_syntastic_c_oclint_checker')
    finish
endif
let g:loaded_syntastic_c_oclint_checker = 1

if !exists('g:syntastic_oclint_config_file')
    let g:syntastic_oclint_config_file = '.syntastic_oclint_config'
endif

if !exists('g:syntastic_c_oclint_sort')
    let g:syntastic_c_oclint_sort = 1
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_c_oclint_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'post_args': '-- -c ' . syntastic#c#ReadConfig(g:syntastic_oclint_config_file) })

    let errorformat =
        \ '%E%f:%l:%c: fatal error: %m,' .
        \ '%E%f:%l:%c: error: %m,' .
        \ '%W%f:%l:%c: warning: %m,' .
        \ '%E%f:%l:%c: %m,' .
        \ '%-G%.%#'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'subtype': 'Style',
        \ 'postprocess': ['compressWhitespace'],
        \ 'returns': [0, 3, 5] })

    for e in loclist
        if e['text'] =~# '\v P3( |$)'
            let e['type'] = 'W'
        endif

        let e['text'] = substitute(e['text'], '\m\C P[1-3]$', '', '')
        let e['text'] = substitute(e['text'], '\m\C P[1-3] ', ': ', '')
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'c',
    \ 'name': 'oclint'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
