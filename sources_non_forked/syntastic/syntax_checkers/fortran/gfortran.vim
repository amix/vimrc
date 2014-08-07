"============================================================================
"File:        fortran.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Karl Yngve Lervåg <karl.yngve@lervag.net>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_fortran_gfortran_checker")
    finish
endif
let g:loaded_syntastic_fortran_gfortran_checker=1

if !exists('g:syntastic_fortran_compiler_options')
    let g:syntastic_fortran_compiler_options = ''
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_fortran_gfortran_IsAvailable() dict
    if !exists('g:syntastic_fortran_compiler')
        let g:syntastic_fortran_compiler = self.getExec()
    endif
    return executable(expand(g:syntastic_fortran_compiler))
endfunction

function! SyntaxCheckers_fortran_gfortran_GetLocList() dict
    return syntastic#c#GetLocList('fortran', 'gfortran', {
        \ 'errorformat':
        \     '%-C %#,'.
        \     '%-C  %#%.%#,'.
        \     '%A%f:%l.%c:,'.
        \     '%Z%trror: %m,'.
        \     '%Z%tarning: %m,'.
        \     '%-G%.%#',
        \ 'main_flags': '-fsyntax-only' })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'fortran',
    \ 'name': 'gfortran' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
