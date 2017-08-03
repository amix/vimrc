"============================================================================
"File:        flawfinder.vim
"Description: Syntax checking plugin for syntastic
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_c_flawfinder_checker')
    finish
endif
let g:loaded_syntastic_c_flawfinder_checker = 1

if !exists('g:syntastic_c_flawfinder_sort')
    let g:syntastic_c_flawfinder_sort = 1
endif

if !exists('g:syntastic_c_flawfinder_thres')
    let g:syntastic_c_flawfinder_thres = 3
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_c_flawfinder_GetHighlightRegex(item)
    let term = matchstr(a:item['text'], '\m^(\S\+)\s\+\zs\S\+\ze:')
    return term !=# '' ? '\V\<' . escape(term, '\') . '\>' : ''
endfunction

function! SyntaxCheckers_c_flawfinder_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'args_after': '--columns --dataonly --singleline --quiet' })

    let errorformat = '%f:%l:%c:  [%n] %m'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'subtype': 'Style',
        \ 'returns': [0] })

    for e in loclist
        let e['type'] = e['nr'] < g:syntastic_{self.getFiletype()}_flawfinder_thres ? 'W' : 'E'
        let e['nr'] = 0
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'c',
    \ 'name': 'flawfinder' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
