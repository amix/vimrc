"============================================================================
"File:        pep8.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================
"
" For details about pep8 see: https://github.com/jcrocholl/pep8

if exists("g:loaded_syntastic_python_pep8_checker")
    finish
endif
let g:loaded_syntastic_python_pep8_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_python_pep8_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'exe_before': (syntastic#util#isRunningWindows() ? '' : 'TERM=dumb') })

    let errorformat = '%f:%l:%c: %m'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'subtype': 'Style' })

    for e in loclist
        let e['type'] = e['text'] =~? '^W' ? 'W' : 'E'
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'python',
    \ 'name': 'pep8'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
