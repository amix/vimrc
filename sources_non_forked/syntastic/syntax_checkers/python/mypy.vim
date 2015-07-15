"============================================================================
"File:        mypy.vim
"Description: Syntax checking plugin for syntastic.vim
"Author:      Russ Hewgill <Russ dot Hewgill at gmail dot com>
"
"============================================================================

if exists('g:loaded_syntastic_python_mypy_checker')
    finish
endif
let g:loaded_syntastic_python_mypy_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_python_mypy_GetLocList() dict
    let makeprg = self.makeprgBuild({})

    let errorformat = '%f\, line %l: %m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'defaults': { 'type': 'E' },
        \ 'returns': [0, 1] })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'python',
    \ 'name': 'mypy'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
