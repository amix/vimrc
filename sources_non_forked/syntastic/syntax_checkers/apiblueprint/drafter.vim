"============================================================================
"File:        drafter.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_apiblueprint_drafter_checker')
    finish
endif
let g:loaded_syntastic_apiblueprint_drafter_checker = 1

if !exists('g:syntastic_apiblueprint_drafter_sort')
    let g:syntastic_apiblueprint_drafter_sort = 1
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_apiblueprint_drafter_GetLocList() dict
    let makeprg = self.makeprgBuild({ 'post_args': '-u -l' })

    let errorformat =
        \ '%trror: (%n)  %m,' .
        \ '%tarning: (%n)  %m,' .
        \ '%-G%.%#'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'defaults': {'bufnr': bufnr('')},
        \ 'returns': [0, 2, 3, 4] })

    for e in loclist
        let matches = matchlist(e['text'], '\v^(.+); line (\d+), column (\d+) - line (\d+), column (\d+)$')
        if len(matches) > 5
            let e['lnum'] = str2nr(matches[2])
            let e['col']  = str2nr(matches[3])
            let e['vcol'] = 0

            if matches[2] == matches[4]
                let e['hl'] = '\%>' . (e['col'] - 1) . 'c\%<' . matches[5] . 'c'
            endif

            let e['text'] = matches[1]
        else
            let e['valid'] = 0
        endif
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'apiblueprint',
    \ 'name': 'drafter'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
