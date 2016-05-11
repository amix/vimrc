"============================================================================
"File:        pydocstyle.vim
"Description: Syntax checking plugin for syntastic
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_python_pydocstyle_checker')
    finish
endif
let g:loaded_syntastic_python_pydocstyle_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_python_pydocstyle_GetLocList() dict
    if !exists('s:pydocstyle_new')
        let s:pydocstyle_new = syntastic#util#versionIsAtLeast(self.getVersion(), [0, 3])
    endif

    let makeprg = self.makeprgBuild({})

    if s:pydocstyle_new
        let errorformat =
            \ '%E%f:%l %.%#:,' .
            \ '%+C        %m'
    else
        let errorformat =
            \ '%E%f:%l:%c%\%.%\%.%\d%\+:%\d%\+: %m,' .
            \ '%E%f:%l:%c: %m,' .
            \ '%+C    %m'
    endif

    let env = syntastic#util#isRunningWindows() ? {} : { 'TERM': 'dumb' }

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'env': env,
        \ 'subtype': 'Style',
        \ 'preprocess': 'killEmpty',
        \ 'postprocess': ['compressWhitespace'] })

    if s:pydocstyle_new == 0
        " byte offsets rather than column numbers
        for e in loclist
            let e['col'] = get(e, 'col', 0) + 1
        endfor
    endif

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'python',
    \ 'name': 'pydocstyle'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
