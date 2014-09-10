"============================================================================
"File:        py3kwarn.vim
"Description: Syntax checking plugin for syntastic.vim
"Authors:     Liam Curry <liam@curry.name>
"
"============================================================================

if exists("g:loaded_syntastic_python_py3kwarn_checker")
    finish
endif
let g:loaded_syntastic_python_py3kwarn_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_python_py3kwarn_GetLocList() dict
    let makeprg = self.makeprgBuild({})

    let errorformat = '%W%f:%l:%c: %m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'python',
    \ 'name': 'py3kwarn'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
