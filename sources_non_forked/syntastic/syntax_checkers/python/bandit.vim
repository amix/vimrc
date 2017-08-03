"============================================================================
"File:        bandit
"Description: Syntax checking plugin for syntastic
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_python_bandit_checker')
    finish
endif
let g:loaded_syntastic_python_bandit_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_python_bandit_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'args_after': '--format json',
        \ 'tail': '2> ' . syntastic#util#DevNull() })

    let errorformat = '%f:%l:%t:%n:%m'

    let env = syntastic#util#isRunningWindows() ? {} : { 'TERM': 'dumb' }

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'env': env,
        \ 'preprocess': 'bandit',
        \ 'returns': [0, 1] })

    for e in loclist
        if e['type'] ==? 'I'
            let e['type'] = 'W'
            let e['subtype'] = 'Style'
        endif
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'python',
    \ 'name': 'bandit' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
