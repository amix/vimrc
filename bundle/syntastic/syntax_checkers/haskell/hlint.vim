"============================================================================
"File:        hlint.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Nicolas Wu <nicolas.wu at gmail dot com>
"License:     BSD
"============================================================================

if exists('g:loaded_syntastic_haskell_hlint_checker')
    finish
endif
let g:loaded_syntastic_haskell_hlint_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_haskell_hlint_GetLocList() dict
    let makeprg = self.makeprgBuild({})

    let errorformat =
        \ '%E%f:%l:%c: Error: %m,' .
        \ '%W%f:%l:%c: Warning: %m,' .
        \ '%C%m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'postprocess': ['compressWhitespace'] })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'haskell',
    \ 'name': 'hlint'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
