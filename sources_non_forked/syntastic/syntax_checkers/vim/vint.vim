"============================================================================
"File:        vint.vim
"Description: Syntax checking plugin for syntastic
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_vim_vint_checker')
    finish
endif
let g:loaded_syntastic_vim_vint_checker = 1

if !exists('g:syntastic_vim_vint_sort')
    let g:syntastic_vim_vint_sort = 1
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_vim_vint_GetLocList() dict
    let makeprg = self.makeprgBuild({ 'post_args': '--json' })

    let errorformat = '%f:%l:%c:%t: %m'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'preprocess': 'vint',
        \ 'returns': [0, 1] })

    for e in loclist
        if e['type'] ==? 's'
            let e['type'] = 'w'
            let e['subtype'] = 'Style'
        elseif e['type'] !=? 'e' && e['type'] !=? 'w'
            let e['type'] = 'e'
        endif
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'vim',
    \ 'name': 'vint'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
