"============================================================================
"File:        flow.vim
"Description: Javascript syntax checker - using flow
"Maintainer:  Michael Robinson <mike@pagesofinterest.net>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"============================================================================

if exists('g:loaded_syntastic_javascript_flow_checker')
    finish
endif
let g:loaded_syntastic_javascript_flow_checker = 1

if !exists('g:syntastic_javascript_flow_sort')
    let g:syntastic_javascript_flow_sort = 1
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_javascript_flow_IsAvailable() dict
    if !executable(self.getExec())
        return 0
    endif
    return syntastic#util#versionIsAtLeast(self.getVersion(self.getExecEscaped() . ' version'), [0, 18, 1])
endfunction

function! SyntaxCheckers_javascript_flow_GetLocList() dict
    if syntastic#util#findFileInParent('.flowconfig', expand('%:p:h', 1)) ==# ''
        return []
    endif

    let makeprg = self.makeprgBuild({
        \ 'exe': self.getExecEscaped() . ' check',
        \ 'args_after': '--show-all-errors --json' })

    let errorformat =
        \ '%f:%l:%c:%n: %m,' .
        \ '%f:%l:%c: %m'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'preprocess': 'flow',
        \ 'defaults': {'type': 'E'} })

    for e in loclist
        if get(e, 'col', 0) && get(e, 'nr', 0)
            let e['hl'] = '\%>' . (e['col'] - 1) . 'c\%<' . (e['nr'] + 1) . 'c'
            let e['nr'] = 0
        endif
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'javascript',
    \ 'name': 'flow'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
