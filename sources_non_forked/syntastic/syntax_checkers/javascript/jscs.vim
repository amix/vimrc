"============================================================================
"File:        jscs.vim
"Description: Javascript syntax checker - using jscs
"Maintainer:  LCD 47 <lcd047@gmail.com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"============================================================================

if exists('g:loaded_syntastic_javascript_jscs_checker')
    finish
endif
let g:loaded_syntastic_javascript_jscs_checker = 1

if !exists('g:syntastic_javascript_jscs_sort')
    let g:syntastic_javascript_jscs_sort = 1
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_javascript_jscs_IsAvailable() dict
    if !executable(self.getExec())
        return 0
    endif
    return syntastic#util#versionIsAtLeast(self.getVersion(), [2, 1])
endfunction

function! SyntaxCheckers_javascript_jscs_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'args_after': '--no-colors --max-errors -1 --reporter json' })

    let errorformat = '%f:%l:%c:%m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'subtype': 'Style',
        \ 'preprocess': 'jscs',
        \ 'defaults': {'type': 'E'},
        \ 'returns': [0, 2] })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'javascript',
    \ 'name': 'jscs'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
