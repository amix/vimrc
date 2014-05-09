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
"
" The setting 'g:syntastic_oclint_config_file' allows you to define a file
" that contains additional compiler arguments like include directories or
" CFLAGS. The file is expected to contain one option per line. If none is
" given the filename defaults to '.syntastic_oclint_config':
"
"   let g:syntastic_oclint_config_file = '.config'

if exists("g:loaded_syntastic_c_oclint_checker")
    finish
endif
let g:loaded_syntastic_c_oclint_checker = 1

if !exists('g:syntastic_oclint_config_file')
    let g:syntastic_oclint_config_file = '.syntastic_oclint_config'
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_c_oclint_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'args_after': '-text',
        \ 'post_args_before': '-- -c ' . syntastic#c#ReadConfig(g:syntastic_oclint_config_file) })

    let errorformat =
        \ '%E%f:%l:%c: %m P1 ,' .
        \ '%E%f:%l:%c: %m P2 ,' .
        \ '%W%f:%l:%c: %m P3 ,' .
        \ '%E%f:%l:%c: fatal error: %m,' .
        \ '%E%f:%l:%c: error: %m,' .
        \ '%W%f:%l:%c: warning: %m,' .
        \ '%-G%.%#'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'subtype': 'Style',
        \ 'postprocess': ['compressWhitespace', 'sort'],
        \ 'returns': [0, 3, 5] })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'c',
    \ 'name': 'oclint'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
