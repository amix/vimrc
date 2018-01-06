"============================================================================
"File:        verapp.vim
"Description: Syntax checking plugin for syntastic
"Maintainer:  Lucas Verney <phyks@phyks.me>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
" Tested with Vera++ 1.3.0
"============================================================================

if exists('g:loaded_syntastic_cpp_verapp_checker')
    finish
endif
let g:loaded_syntastic_cpp_verapp_checker = 1

if !exists('g:syntastic_verapp_config_file')
    let g:syntastic_verapp_config_file = '.syntastic_verapp_config'
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_cpp_verapp_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'args': syntastic#c#ReadConfig(g:syntastic_verapp_config_file),
        \ 'args_after': '--show-rule --no-duplicate -S -c -' })

    let errorformat = '%f:%t:%l:%c:%m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'preprocess': 'checkstyle',
        \ 'subtype': 'Style' })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'cpp',
    \ 'name': 'verapp',
    \ 'exec': 'vera++'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
