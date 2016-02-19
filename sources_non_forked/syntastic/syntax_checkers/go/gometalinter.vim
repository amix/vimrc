"============================================================================
"File:        gometalinter.vim
"Description: Check go syntax using 'gometalint'
"Maintainer:  Joshua Rubin <joshua@rubixconsulting.com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_go_gometalinter_checker')
    finish
endif
let g:loaded_syntastic_go_gometalinter_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_go_gometalinter_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'args': '-t',
        \ 'fname': syntastic#util#shexpand('%:p:h') })

    let errorformat =
        \ '%f:%l:%c:%trror: %m,' .
        \ '%f:%l:%c:%tarning: %m,' .
        \ '%f:%l::%trror: %m,' .
        \ '%f:%l::%tarning: %m'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'returns': [0, 1] })

    for e in loclist
        if e['text'] =~# '\v\(%(deadcode|gocyclo|golint|defercheck|varcheck|structcheck|errcheck|dupl)\)$'
            let e['subtype'] = 'Style'
        endif
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'go',
    \ 'name': 'gometalinter'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
